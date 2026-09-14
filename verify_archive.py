#!/usr/bin/env python3
"""
CIMC / dashboard archive verifier

Usage:
    python3 verify_archive.py tsa_verification.zip dashboard.zip

You may give the two ZIP files in either order. The script identifies the
timestamp-evidence ZIP automatically.

What it verifies:
  1. The timestamp evidence bundle is structurally complete.
  2. The embedded TSA trust root matches an approved, pinned FreeTSA root.
  3. The RFC3161 timestamp cryptographically verifies against manifest.json.
  4. The supplied payload ZIP has the SHA-256 and byte size recorded in the manifest.
  5. Every file inside the payload ZIP has the size and SHA-256 recorded in the manifest.
  6. No expected files are missing and no unexpected files are present.

Exit codes:
    0 = verification passed (warnings may still be present)
    1 = verification failed
    2 = usage/dependency/input error
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import tempfile
import zipfile
from collections import Counter
from datetime import timezone
from email.utils import parsedate_to_datetime
from pathlib import Path
from typing import Any


# FreeTSA root CA currently used by the live CIMC v1.0 implementation.
#
# This fingerprint is deliberately pinned in the verifier rather than trusting
# any arbitrary root certificate supplied inside the archive itself.
#
# Subject:
#   O=Free TSA, OU=Root CA, CN=www.freetsa.org, ...
#
# SHA-256 fingerprint:
#   A6:37:9E:7C:EC:C0:5F:AA:3C:BF:07:60:13:D7:45:E3:
#   27:BB:BA:A3:8C:0B:9A:F2:24:69:D4:70:1D:18:AA:BC
APPROVED_TSA_ROOT_SHA256 = {
    "A6379E7CECC05FAA3CBF076013D745E327BBBAA38C0B9AF22469D4701D18AABC",
}


class Verification:
    def __init__(self) -> None:
        self.failures: list[str] = []
        self.warnings: list[str] = []
        self.notes: list[str] = []
        self.tech: dict[str, Any] = {}
        self.checks: list[tuple[str, bool, str]] = []

    def fail(self, message: str) -> None:
        self.failures.append(message)

    def warn(self, message: str) -> None:
        self.warnings.append(message)

    def note(self, message: str) -> None:
        self.notes.append(message)

    def check(self, name: str, passed: bool, detail: str) -> None:
        self.checks.append((name, passed, detail))
        if not passed:
            self.fail(detail)

    @property
    def passed(self) -> bool:
        return not self.failures


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def sha256_stream(stream) -> str:
    h = hashlib.sha256()
    while True:
        block = stream.read(1024 * 1024)
        if not block:
            break
        h.update(block)
    return h.hexdigest()


def run(cmd: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, capture_output=True, text=True, check=False)


def normalise_fingerprint(value: str) -> str:
    return "".join(c for c in value.upper() if c in "0123456789ABCDEF")


def find_member(zf: zipfile.ZipFile, basename: str) -> str | None:
    matches = [
        name
        for name in zf.namelist()
        if not name.endswith("/") and Path(name).name == basename
    ]
    if len(matches) == 1:
        return matches[0]
    return None


def identify_inputs(a: Path, b: Path) -> tuple[Path | None, Path | None, str | None]:
    """Return (tsa_zip, payload_zip, error)."""

    def looks_like_tsa(path: Path) -> bool:
        if not zipfile.is_zipfile(path):
            return False
        try:
            with zipfile.ZipFile(path) as zf:
                names = {Path(n).name for n in zf.namelist() if not n.endswith("/")}
                return "manifest.json" in names and "response.tsr" in names
        except zipfile.BadZipFile:
            return False

    a_tsa = looks_like_tsa(a)
    b_tsa = looks_like_tsa(b)

    if a_tsa and not b_tsa:
        return a, b, None
    if b_tsa and not a_tsa:
        return b, a, None
    if a_tsa and b_tsa:
        return None, None, "Both inputs look like timestamp-evidence ZIP files."
    return None, None, (
        "Neither input looks like a timestamp-evidence ZIP. "
        "The TSA ZIP must contain manifest.json and response.tsr."
    )


def extract_named_member(
    zf: zipfile.ZipFile,
    basename: str,
    destination: Path,
    result: Verification,
) -> Path | None:
    member = find_member(zf, basename)
    if member is None:
        result.fail(
            f"Timestamp evidence is missing a unique {basename!r} file."
        )
        return None

    out = destination / basename
    try:
        out.write_bytes(zf.read(member))
    except Exception as exc:
        result.fail(f"Could not read {basename} from the timestamp evidence: {exc}")
        return None
    return out


def certificate_info(openssl: str, cert: Path) -> dict[str, str]:
    proc = run(
        [
            openssl,
            "x509",
            "-in",
            str(cert),
            "-noout",
            "-subject",
            "-issuer",
            "-serial",
            "-dates",
            "-fingerprint",
            "-sha256",
        ]
    )
    info: dict[str, str] = {}
    if proc.returncode != 0:
        return info

    for raw in proc.stdout.splitlines():
        line = raw.strip()
        if "=" not in line:
            continue
        key, value = line.split("=", 1)
        info[key.strip()] = value.strip()
    return info


def root_fingerprint(openssl: str, cert: Path) -> str | None:
    proc = run(
        [
            openssl,
            "x509",
            "-in",
            str(cert),
            "-noout",
            "-fingerprint",
            "-sha256",
        ]
    )
    if proc.returncode != 0:
        return None
    for line in proc.stdout.splitlines():
        if "Fingerprint=" in line:
            return normalise_fingerprint(line.split("=", 1)[1])
    return None


def timestamp_details(openssl: str, response: Path) -> tuple[dict[str, str], str]:
    proc = run([openssl, "ts", "-reply", "-in", str(response), "-text"])
    details: dict[str, str] = {}
    text = (proc.stdout or "") + (proc.stderr or "")

    if proc.returncode != 0:
        return details, text

    keys = {
        "Policy OID": "policy_oid",
        "Hash Algorithm": "hash_algorithm",
        "Serial number": "serial_number",
        "Time stamp": "timestamp",
        "Accuracy": "accuracy",
        "Ordering": "ordering",
        "Nonce": "nonce",
        "TSA": "tsa",
        "Status": "status",
    }

    for raw in proc.stdout.splitlines():
        line = raw.strip()
        for prefix, key in keys.items():
            marker = prefix + ":"
            if line.startswith(marker):
                details[key] = line[len(marker):].strip()

    return details, text


def timestamp_epoch(timestamp_text: str | None) -> int | None:
    if not timestamp_text:
        return None
    try:
        dt = parsedate_to_datetime(timestamp_text)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return int(dt.timestamp())
    except Exception:
        return None


def verify(args: list[str]) -> tuple[Verification, int]:
    result = Verification()

    parser = argparse.ArgumentParser(
        description=(
            "Verify a dashboard/archive payload against its RFC3161 timestamp "
            "evidence. Supply the two ZIP files in either order."
        )
    )
    parser.add_argument("zip1", type=Path, help="tsa_verification.zip or payload ZIP")
    parser.add_argument("zip2", type=Path, help="payload ZIP or tsa_verification.zip")
    ns = parser.parse_args(args)

    zip1 = ns.zip1.expanduser().resolve()
    zip2 = ns.zip2.expanduser().resolve()

    result.tech["input_1"] = str(zip1)
    result.tech["input_2"] = str(zip2)

    for path in (zip1, zip2):
        if not path.is_file():
            result.fail(f"Input file does not exist: {path}")
            return result, 2
        if not zipfile.is_zipfile(path):
            result.fail(f"Input is not a readable ZIP file: {path}")
            return result, 2

    tsa_zip, payload_zip, identity_error = identify_inputs(zip1, zip2)
    if identity_error:
        result.fail(identity_error)
        return result, 2

    assert tsa_zip is not None
    assert payload_zip is not None

    result.tech["tsa_zip"] = str(tsa_zip)
    result.tech["payload_zip"] = str(payload_zip)

    openssl = shutil.which("openssl")
    if not openssl:
        result.fail(
            "OpenSSL was not found. RFC3161 timestamp verification cannot be performed."
        )
        return result, 2

    version_proc = run([openssl, "version"])
    result.tech["openssl"] = (
        version_proc.stdout.strip()
        or version_proc.stderr.strip()
        or openssl
    )

    ts_help = run([openssl, "ts", "-help"])
    if ts_help.returncode != 0 and "ts" not in (ts_help.stdout + ts_help.stderr).lower():
        result.fail(
            "The installed OpenSSL does not appear to support RFC3161 "
            "timestamp verification ('openssl ts')."
        )
        return result, 2

    with tempfile.TemporaryDirectory(prefix="archive_verify_") as td_raw:
        td = Path(td_raw)

        try:
            with zipfile.ZipFile(tsa_zip) as zf:
                result.tech["tsa_bundle_members"] = [
                    n for n in zf.namelist() if not n.endswith("/")
                ]
                manifest_path = extract_named_member(zf, "manifest.json", td, result)
                response_path = extract_named_member(zf, "response.tsr", td, result)
                root_path = extract_named_member(zf, "trusted_roots.pem", td, result)
                intermediate_path = extract_named_member(
                    zf, "intermediates.pem", td, result
                )
                request_member = find_member(zf, "request.tsq")
                verification_member = find_member(zf, "verification.txt")
                details_member = find_member(zf, "timestamp_details.txt")
                result.tech["request_tsq_present"] = request_member is not None
                result.tech["verification_txt_present"] = verification_member is not None
                result.tech["timestamp_details_txt_present"] = details_member is not None
        except zipfile.BadZipFile:
            result.fail("The timestamp-evidence file is not a valid ZIP archive.")
            return result, 1

        if result.failures:
            return result, 1

        assert manifest_path is not None
        assert response_path is not None
        assert root_path is not None
        assert intermediate_path is not None

        try:
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        except Exception as exc:
            result.fail(f"manifest.json is not valid JSON: {exc}")
            return result, 1

        result.tech["manifest_sha256"] = sha256_file(manifest_path)
        result.tech["manifest_schema_version"] = manifest.get("schema_version")
        result.tech["project"] = manifest.get("project")
        result.tech["audience"] = manifest.get("audience")
        result.tech["run_id"] = manifest.get("run_id")
        result.tech["created_utc"] = manifest.get("created_utc")
        result.tech["controller_version"] = manifest.get("controller_version")
        result.tech["source_mode"] = manifest.get("source_mode")
        result.tech["scope"] = manifest.get("scope")

        # ------------------------------------------------------------
        # Trust anchor pinning
        # ------------------------------------------------------------
        fingerprint = root_fingerprint(openssl, root_path)
        result.tech["tsa_root_sha256_fingerprint"] = fingerprint

        if not fingerprint:
            result.fail("Could not read the TSA root certificate fingerprint.")
        elif fingerprint not in APPROVED_TSA_ROOT_SHA256:
            result.fail(
                "The TSA root certificate is not one of the trusted roots pinned "
                "in this verifier. Do not accept the archive until the new root "
                "has been independently reviewed."
            )
        else:
            result.check(
                "tsa_root",
                True,
                "The TSA root certificate matches the approved FreeTSA trust anchor.",
            )

        result.tech["tsa_root_certificate"] = certificate_info(openssl, root_path)
        result.tech["tsa_signing_certificate"] = certificate_info(
            openssl, intermediate_path
        )

        # ------------------------------------------------------------
        # Timestamp details and verification
        # ------------------------------------------------------------
        ts_details, ts_details_raw = timestamp_details(openssl, response_path)
        result.tech["timestamp"] = ts_details
        result.tech["timestamp_details_raw"] = ts_details_raw

        verify_cmd = [
            openssl,
            "ts",
            "-verify",
            "-data",
            str(manifest_path),
            "-in",
            str(response_path),
            "-CAfile",
            str(root_path),
            "-untrusted",
            str(intermediate_path),
        ]

        # Verify certificate validity at the certified timestamp when supported.
        epoch = timestamp_epoch(ts_details.get("timestamp"))
        if epoch is not None:
            verify_cmd.extend(["-attime", str(epoch)])
            result.tech["verification_attime_epoch"] = epoch

        ts_verify = run(verify_cmd)
        result.tech["timestamp_verification_stdout"] = ts_verify.stdout.strip()
        result.tech["timestamp_verification_stderr"] = ts_verify.stderr.strip()

        if ts_verify.returncode == 0 and "Verification: OK" in ts_verify.stdout:
            result.check(
                "timestamp",
                True,
                "The RFC3161 timestamp cryptographically verifies against the exact manifest.",
            )
        else:
            result.fail(
                "The RFC3161 timestamp does not verify against the manifest. "
                "The archive must not be treated as verified."
            )

        # ------------------------------------------------------------
        # Whole payload verification
        # ------------------------------------------------------------
        package = manifest.get("package")
        if not isinstance(package, dict):
            result.fail("The manifest does not contain a valid 'package' record.")
            package = {}

        expected_name = package.get("filename")
        expected_size = package.get("bytes")
        expected_hash = package.get("sha256")

        actual_size = payload_zip.stat().st_size
        actual_hash = sha256_file(payload_zip)

        result.tech["payload_expected_filename"] = expected_name
        result.tech["payload_actual_filename"] = payload_zip.name
        result.tech["payload_expected_bytes"] = expected_size
        result.tech["payload_actual_bytes"] = actual_size
        result.tech["payload_expected_sha256"] = expected_hash
        result.tech["payload_actual_sha256"] = actual_hash

        if expected_hash:
            if actual_hash == expected_hash:
                result.check(
                    "payload_hash",
                    True,
                    "The payload ZIP SHA-256 matches the manifest.",
                )
            else:
                result.fail(
                    "The payload ZIP has the wrong SHA-256 hash. It is not the "
                    "exact package recorded by the timestamped manifest."
                )
        else:
            result.fail("The manifest does not contain the payload SHA-256.")

        if isinstance(expected_size, int):
            if actual_size == expected_size:
                result.check(
                    "payload_size",
                    True,
                    "The payload ZIP byte size matches the manifest.",
                )
            else:
                result.fail(
                    f"The payload ZIP size is {actual_size:,} bytes but the "
                    f"manifest records {expected_size:,} bytes."
                )
        else:
            result.fail("The manifest does not contain a valid payload byte size.")

        if expected_name and payload_zip.name != expected_name:
            result.warn(
                f"The payload file has been renamed from {expected_name!r} to "
                f"{payload_zip.name!r}. Its cryptographic hash is still checked, "
                "so a rename alone does not invalidate it."
            )

        # ------------------------------------------------------------
        # Per-file verification inside payload ZIP
        # ------------------------------------------------------------
        records = manifest.get("files")
        if not isinstance(records, list):
            result.fail("The manifest does not contain a valid per-file list.")
            records = []

        expected: dict[str, dict[str, Any]] = {}
        bad_manifest_records = 0
        for rec in records:
            if not isinstance(rec, dict):
                bad_manifest_records += 1
                continue
            filename = rec.get("filename")
            if not isinstance(filename, str) or not filename:
                bad_manifest_records += 1
                continue
            if filename in expected:
                result.fail(
                    f"The manifest contains duplicate file records for {filename!r}."
                )
            expected[filename] = rec

        if bad_manifest_records:
            result.fail(
                f"The manifest contains {bad_manifest_records} invalid file record(s)."
            )

        verified_files = 0
        content_failures: list[str] = []

        try:
            with zipfile.ZipFile(payload_zip) as pzf:
                names = [
                    info.filename
                    for info in pzf.infolist()
                    if not info.is_dir()
                ]
                duplicates = sorted(
                    name for name, count in Counter(names).items() if count > 1
                )
                if duplicates:
                    result.fail(
                        "The payload ZIP contains duplicate filenames, making the "
                        "archive ambiguous: " + ", ".join(duplicates[:10])
                    )

                actual_names = set(names)
                expected_names = set(expected)

                missing = sorted(expected_names - actual_names)
                extra = sorted(actual_names - expected_names)

                result.tech["expected_internal_file_count"] = len(expected_names)
                result.tech["actual_internal_file_count"] = len(actual_names)
                result.tech["missing_internal_files"] = missing
                result.tech["extra_internal_files"] = extra
                result.tech["duplicate_internal_files"] = duplicates

                if missing:
                    result.fail(
                        f"{len(missing)} file(s) recorded in the manifest are "
                        "missing from the payload ZIP."
                    )
                if extra:
                    result.fail(
                        f"{len(extra)} unexpected file(s) are present in the "
                        "payload ZIP but absent from the manifest."
                    )

                for filename in sorted(expected_names & actual_names):
                    rec = expected[filename]

                    try:
                        info = pzf.getinfo(filename)
                    except KeyError:
                        content_failures.append(f"{filename}: missing")
                        continue

                    expected_bytes = rec.get("bytes")
                    expected_file_hash = rec.get("sha256")

                    if info.file_size != expected_bytes:
                        content_failures.append(
                            f"{filename}: size {info.file_size} != {expected_bytes}"
                        )
                        continue

                    try:
                        with pzf.open(info, "r") as fh:
                            digest = sha256_stream(fh)
                    except Exception as exc:
                        content_failures.append(f"{filename}: could not read ({exc})")
                        continue

                    if digest != expected_file_hash:
                        content_failures.append(
                            f"{filename}: SHA-256 mismatch"
                        )
                        continue

                    verified_files += 1

        except zipfile.BadZipFile:
            result.fail("The payload is not a valid ZIP archive.")

        result.tech["verified_internal_files"] = verified_files
        result.tech["internal_file_failures"] = content_failures

        if content_failures:
            result.fail(
                f"{len(content_failures)} file(s) inside the payload failed their "
                "size or SHA-256 check."
            )

        if (
            not result.failures
            and verified_files == len(expected)
            and len(expected) > 0
        ):
            result.check(
                "internal_files",
                True,
                f"All {verified_files} files inside the payload match the timestamped manifest.",
            )

    return result, (0 if result.passed else 1)


def print_report(result: Verification) -> None:
    tech = result.tech

    # ============================================================
    # 1. Plain-English result
    # ============================================================
    print()
    print("=" * 72)
    print("1. PLAIN-ENGLISH RESULT")
    print("=" * 72)

    if result.passed:
        timestamp = (tech.get("timestamp") or {}).get("timestamp")
        count = tech.get("verified_internal_files")

        print()
        print("✅ VERIFIED — this archive passed the integrity and timestamp checks.")
        print()
        print(
            "In plain English: the supplied archive is the same package described "
            "by the manifest, and the manifest is protected by a valid trusted "
            "timestamp."
        )
        if timestamp:
            print(
                f"The timestamp authority certified the manifest at {timestamp}. "
                "This provides evidence that this exact manifest existed by that time."
            )
        if isinstance(count, int) and count > 0:
            print(
                f"Every one of the {count} files inside the payload ZIP also "
                "matches the file-level hashes recorded in that manifest."
            )
        print()
        print(
            "Nothing in these checks indicates that the archive has been changed "
            "since it was created."
        )
    else:
        print()
        print("❌ NOT VERIFIED — one or more important checks failed.")
        print()
        print(
            "Do not treat this archive as an authenticated copy of the original "
            "delivery until the problems below have been resolved."
        )
        print(
            "A failure does not automatically prove deliberate tampering: it can "
            "also result from downloading the wrong pair of ZIP files, an incomplete "
            "download, corruption, or using evidence from a different run."
        )

    project = tech.get("project")
    audience = tech.get("audience")
    run_id = tech.get("run_id")
    if project or audience or run_id:
        print()
        print("Archive identified as:")
        if project:
            print(f"  Project:  {project}")
        if audience:
            print(f"  Audience: {audience}")
        if run_id:
            print(f"  Run ID:   {run_id}")

    # ============================================================
    # 2. Issues and warnings
    # ============================================================
    print()
    print("=" * 72)
    print("2. ISSUES AND WARNINGS")
    print("=" * 72)
    print()

    if not result.failures and not result.warnings:
        print("✅ No issues or warnings were found.")
    else:
        if result.failures:
            print("PROBLEMS THAT CAUSED VERIFICATION TO FAIL:")
            for i, message in enumerate(result.failures, 1):
                print(f"  {i}. ❌ {message}")
            print()
            print("What to do:")
            print(
                "  • Download both ZIP files again from the same archive submission."
            )
            print(
                "  • Make sure one file is the TSA verification ZIP and the other "
                "is its matching payload ZIP."
            )
            print(
                "  • Run this verifier again. If the same failure remains, preserve "
                "the files and ask the data-management team to investigate."
            )

        if result.warnings:
            if result.failures:
                print()
            print("WARNINGS:")
            for i, message in enumerate(result.warnings, 1):
                print(f"  {i}. ⚠️  {message}")

    # ============================================================
    # 3. Technical report
    # ============================================================
    print()
    print("=" * 72)
    print("3. TECHNICAL VERIFICATION REPORT")
    print("=" * 72)
    print()

    print(f"Overall result:       {'PASS' if result.passed else 'FAIL'}")
    if tech.get("project") is not None:
        print(f"Project:              {tech.get('project')}")
    if tech.get("audience") is not None:
        print(f"Audience:             {tech.get('audience')}")
    if tech.get("run_id") is not None:
        print(f"Run ID:               {tech.get('run_id')}")
    if tech.get("created_utc") is not None:
        print(f"Manifest created UTC: {tech.get('created_utc')}")
    if tech.get("controller_version") is not None:
        print(f"Controller version:   {tech.get('controller_version')}")
    if tech.get("source_mode") is not None:
        print(f"Source mode:          {tech.get('source_mode')}")
    if tech.get("manifest_schema_version") is not None:
        print(f"Manifest schema:      {tech.get('manifest_schema_version')}")

    print()
    print("Inputs")
    print("------")
    if tech.get("tsa_zip"):
        print(f"TSA evidence ZIP:     {tech.get('tsa_zip')}")
    if tech.get("payload_zip"):
        print(f"Payload ZIP:          {tech.get('payload_zip')}")
    if tech.get("openssl"):
        print(f"OpenSSL:              {tech.get('openssl')}")

    print()
    print("Trusted timestamp")
    print("-----------------")
    ts = tech.get("timestamp") or {}
    print(
        f"RFC3161 verification: "
        f"{'PASS' if any(n == 'timestamp' and ok for n, ok, _ in result.checks) else 'FAIL'}"
    )
    if ts.get("status"):
        print(f"TSA response status:  {ts.get('status')}")
    if ts.get("timestamp"):
        print(f"Certified time:       {ts.get('timestamp')}")
    if ts.get("hash_algorithm"):
        print(f"Hash algorithm:       {ts.get('hash_algorithm')}")
    if ts.get("policy_oid"):
        print(f"Policy OID:           {ts.get('policy_oid')}")
    if ts.get("serial_number"):
        print(f"Serial number:        {ts.get('serial_number')}")
    if ts.get("nonce"):
        print(f"Nonce:                {ts.get('nonce')}")
    if ts.get("tsa"):
        print(f"TSA signer:           {ts.get('tsa')}")
    if tech.get("tsa_root_sha256_fingerprint"):
        fp = tech.get("tsa_root_sha256_fingerprint")
        pretty = ":".join(fp[i:i+2] for i in range(0, len(fp), 2))
        print(f"Root SHA-256:         {pretty}")
        print(
            "Pinned root match:    "
            + (
                "PASS"
                if fp in APPROVED_TSA_ROOT_SHA256
                else "FAIL"
            )
        )

    print()
    print("Manifest")
    print("--------")
    if tech.get("manifest_sha256"):
        print(f"Manifest SHA-256:     {tech.get('manifest_sha256')}")
    if tech.get("scope"):
        print(f"Scope:                {tech.get('scope')}")

    print()
    print("Payload package")
    print("---------------")
    if tech.get("payload_expected_filename") is not None:
        print(f"Expected filename:    {tech.get('payload_expected_filename')}")
    if tech.get("payload_actual_filename") is not None:
        print(f"Actual filename:      {tech.get('payload_actual_filename')}")
    if tech.get("payload_expected_bytes") is not None:
        print(f"Expected bytes:       {tech.get('payload_expected_bytes'):,}")
    if tech.get("payload_actual_bytes") is not None:
        print(f"Actual bytes:         {tech.get('payload_actual_bytes'):,}")
    if tech.get("payload_expected_sha256") is not None:
        print(f"Expected SHA-256:     {tech.get('payload_expected_sha256')}")
    if tech.get("payload_actual_sha256") is not None:
        print(f"Actual SHA-256:       {tech.get('payload_actual_sha256')}")

    print()
    print("Files inside payload")
    print("--------------------")
    if tech.get("expected_internal_file_count") is not None:
        print(
            f"Expected file count:  {tech.get('expected_internal_file_count')}"
        )
    if tech.get("actual_internal_file_count") is not None:
        print(
            f"Actual file count:    {tech.get('actual_internal_file_count')}"
        )
    if tech.get("verified_internal_files") is not None:
        print(
            f"Files hash-verified:  {tech.get('verified_internal_files')}"
        )

    missing = tech.get("missing_internal_files") or []
    extra = tech.get("extra_internal_files") or []
    duplicates = tech.get("duplicate_internal_files") or []
    file_failures = tech.get("internal_file_failures") or []

    print(f"Missing files:        {len(missing)}")
    print(f"Unexpected files:     {len(extra)}")
    print(f"Duplicate filenames:  {len(duplicates)}")
    print(f"Content failures:     {len(file_failures)}")

    if missing:
        print("  Missing:")
        for name in missing:
            print(f"    - {name}")
    if extra:
        print("  Unexpected:")
        for name in extra:
            print(f"    - {name}")
    if duplicates:
        print("  Duplicates:")
        for name in duplicates:
            print(f"    - {name}")
    if file_failures:
        print("  File verification failures:")
        for item in file_failures:
            print(f"    - {item}")

    print()
    print("Checks performed")
    print("----------------")
    if result.checks:
        for name, passed, detail in result.checks:
            print(f"{'PASS' if passed else 'FAIL':4}  {name:18} {detail}")
    else:
        print("No checks completed.")

    print()
    print(
        "Interpretation: the RFC3161 check binds the exact manifest to the "
        "certified timestamp. The manifest then binds the payload ZIP and its "
        "individual files through SHA-256 hashes. The TSA root certificate is "
        "also compared with a trusted fingerprint pinned in this verifier."
    )
    print()


def main() -> int:
    result, exit_code = verify(sys.argv[1:])
    print_report(result)
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
