#!/usr/bin/env python3
import argparse
import csv
import datetime as dt
import json
import os
import re
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from flask import Flask, flash, redirect, render_template, request, send_from_directory, url_for


ROOT_DIR = Path(__file__).resolve().parents[1]
SCRIPT_PATH = ROOT_DIR / "scripts" / "scrape_leiloes.R"
LOCAL_PROGRESS_DIR = ROOT_DIR / "local_progress"
RUN_ID_RE = re.compile(r"^[0-9_]+$")
DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")

SITE_VALUES = {"auto", "zuk", "megaleiloes", "leeilon"}
AUCTION_TYPE_VALUES = {"any", "judicial", "extrajudicial"}
ROUND_VALUES = {"any", "1", "2", "ended"}
DATE_FIELD_VALUES = {"next", "first", "second", "any"}
FORMAT_VALUES = {"csv", "json", "xlsx"}

DEFAULT_FORM = {
    "url": "",
    "site": "auto",
    "max_pages": "all",
    "sleep": "0.75",
    "auction_type": "any",
    "current_round": "any",
    "date_from": "",
    "date_to": "",
    "date_field": "next",
    "format": "csv",
    "login_user": "",
    "login_pass": "",
}


app = Flask(__name__)
app.secret_key = os.environ.get("LEILOES_WEB_SECRET", "leiloes-web-local-secret")


def normalize(v: Optional[str]) -> str:
    return (v or "").strip()


def parse_form(form_data) -> Dict[str, str]:
    out = dict(DEFAULT_FORM)
    for key in out:
        out[key] = normalize(form_data.get(key, out[key]))
    out["site"] = out["site"].lower() or "auto"
    out["auction_type"] = out["auction_type"].lower() or "any"
    out["current_round"] = out["current_round"].lower() or "any"
    out["date_field"] = out["date_field"].lower() or "next"
    out["format"] = out["format"].lower() or "csv"
    return out


def validate_form(data: Dict[str, str]) -> List[str]:
    errors: List[str] = []
    if not data["url"]:
        errors.append("URL e obrigatoria.")
    elif not re.match(r"^https?://", data["url"], flags=re.IGNORECASE):
        errors.append("URL deve comecar com http:// ou https://.")

    if data["site"] not in SITE_VALUES:
        errors.append("Site invalido.")
    if data["auction_type"] not in AUCTION_TYPE_VALUES:
        errors.append("auction_type invalido.")
    if data["current_round"] not in ROUND_VALUES:
        errors.append("current_round invalido.")
    if data["date_field"] not in DATE_FIELD_VALUES:
        errors.append("date_field invalido.")
    if data["format"] not in FORMAT_VALUES:
        errors.append("Formato invalido.")

    max_pages = data["max_pages"].lower()
    if max_pages not in {"", "all", "todas", "tudo", "0", "-1", "inf", "infinite", "max"}:
        try:
            n = int(max_pages)
            if n < 1:
                errors.append("max_pages deve ser >= 1 ou 'all'.")
        except ValueError:
            errors.append("max_pages invalido.")

    try:
        sleep_val = float(data["sleep"])
        if sleep_val < 0:
            errors.append("sleep deve ser >= 0.")
    except ValueError:
        errors.append("sleep invalido.")

    if data["date_from"] and not DATE_RE.match(data["date_from"]):
        errors.append("date_from deve estar em YYYY-MM-DD.")
    if data["date_to"] and not DATE_RE.match(data["date_to"]):
        errors.append("date_to deve estar em YYYY-MM-DD.")
    if data["date_from"] and data["date_to"]:
        d1 = dt.date.fromisoformat(data["date_from"])
        d2 = dt.date.fromisoformat(data["date_to"])
        if d1 > d2:
            errors.append("date_from nao pode ser maior que date_to.")

    if (data["login_user"] and not data["login_pass"]) or (data["login_pass"] and not data["login_user"]):
        errors.append("Informe usuario e senha juntos, ou deixe ambos em branco.")

    return errors


def create_run_dir() -> Path:
    LOCAL_PROGRESS_DIR.mkdir(parents=True, exist_ok=True)
    stamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    run_dir = LOCAL_PROGRESS_DIR / stamp
    suffix = 1
    while run_dir.exists():
        run_dir = LOCAL_PROGRESS_DIR / f"{stamp}_{suffix}"
        suffix += 1
    run_dir.mkdir(parents=True, exist_ok=False)
    return run_dir


def build_rscript_args(data: Dict[str, str], out_file: Path) -> List[str]:
    args = [
        "Rscript",
        str(SCRIPT_PATH),
        "--url",
        data["url"],
        "--site",
        data["site"],
        "--max-pages",
        data["max_pages"] or "all",
        "--sleep",
        data["sleep"] or "0.75",
        "--auction-type",
        data["auction_type"],
        "--current-round",
        data["current_round"],
        "--date-field",
        data["date_field"],
        "--format",
        data["format"],
        "--out",
        str(out_file),
        "--verbose",
    ]
    if data["date_from"]:
        args += ["--date-from", data["date_from"]]
    if data["date_to"]:
        args += ["--date-to", data["date_to"]]
    return args


def command_for_log(args: List[str]) -> str:
    return " ".join(f'"{a}"' if " " in a else a for a in args)


def parse_record_count(output: str) -> Optional[int]:
    match = re.search(r"Registros:\s*(\d+)", output)
    if not match:
        return None
    return int(match.group(1))


def read_preview(result_file: Path, fmt: str, limit: int = 120) -> Tuple[List[str], List[Dict[str, str]]]:
    if not result_file.exists():
        return ([], [])

    if fmt == "xlsx":
        return ([], [])

    if fmt == "json":
        with result_file.open("r", encoding="utf-8") as f:
            obj = json.load(f)
        if not isinstance(obj, list):
            return ([], [])
        rows: List[Dict[str, str]] = []
        cols: List[str] = []
        for row in obj[:limit]:
            if not isinstance(row, dict):
                continue
            rows.append({str(k): str(v) if v is not None else "" for k, v in row.items()})
            for k in row.keys():
                if k not in cols:
                    cols.append(str(k))
        return (cols, rows)

    with result_file.open("r", encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        cols = list(reader.fieldnames or [])
        rows = []
        for idx, row in enumerate(reader):
            if idx >= limit:
                break
            rows.append({k: row.get(k, "") or "" for k in cols})
    return (cols, rows)


def read_log_tail(log_file: Path, max_lines: int = 80) -> str:
    if not log_file.exists():
        return ""
    text = log_file.read_text(encoding="utf-8", errors="replace")
    lines = text.splitlines()
    return "\n".join(lines[-max_lines:])


def write_json(path: Path, payload: Dict) -> None:
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def execute_scrape(data: Dict[str, str]) -> Dict:
    run_dir = create_run_dir()
    fmt = data["format"]
    out_file = run_dir / f"resultado.{fmt}"
    log_file = run_dir / "execucao.log"
    meta_file = run_dir / "params.json"

    args = build_rscript_args(data, out_file)
    started = dt.datetime.now(dt.timezone.utc)
    env = os.environ.copy()
    login_enabled = bool(data["login_user"] and data["login_pass"])
    if login_enabled:
        env["LEILOES_LOGIN_USER"] = data["login_user"]
        env["LEILOES_LOGIN_PASS"] = data["login_pass"]
    else:
        env.pop("LEILOES_LOGIN_USER", None)
        env.pop("LEILOES_LOGIN_PASS", None)

    proc = subprocess.run(
        args,
        cwd=str(ROOT_DIR),
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        env=env,
    )
    ended = dt.datetime.now(dt.timezone.utc)
    combined_output = (proc.stdout or "") + ("\n" if proc.stdout and proc.stderr else "") + (proc.stderr or "")

    log_lines = [
        f"[START] {started.isoformat()}",
        f"Comando: {command_for_log(args)}",
        f"Login habilitado: {str(login_enabled).lower()}",
        "",
        combined_output.strip(),
        "",
        f"[END] {ended.isoformat()}",
        f"Exit code: {proc.returncode}",
    ]
    log_file.write_text("\n".join(log_lines), encoding="utf-8")

    metadata = {
        "started_at": started.isoformat(),
        "ended_at": ended.isoformat(),
        "duration_sec": round((ended - started).total_seconds(), 3),
        "exit_code": proc.returncode,
        "url": data["url"],
        "site": data["site"],
        "max_pages": data["max_pages"],
        "sleep": data["sleep"],
        "auction_type": data["auction_type"],
        "current_round": data["current_round"],
        "date_from": data["date_from"],
        "date_to": data["date_to"],
        "date_field": data["date_field"],
        "format": fmt,
        "login_enabled": login_enabled,
        "output_file": str(out_file),
        "log_file": str(log_file),
    }
    write_json(meta_file, metadata)

    columns, preview_rows = read_preview(out_file, fmt=fmt)
    row_count = parse_record_count(combined_output)

    return {
        "run_id": run_dir.name,
        "run_dir": str(run_dir),
        "success": proc.returncode == 0,
        "exit_code": proc.returncode,
        "row_count": row_count,
        "columns": columns,
        "preview_rows": preview_rows,
        "out_file_name": out_file.name,
        "log_file_name": log_file.name,
        "meta_file_name": meta_file.name,
        "log_tail": read_log_tail(log_file),
        "metadata": metadata,
    }


def safe_run_dir(run_id: str) -> Optional[Path]:
    run_id = normalize(run_id)
    if not run_id or not RUN_ID_RE.match(run_id):
        return None
    run_dir = (LOCAL_PROGRESS_DIR / run_id).resolve()
    try:
        run_dir.relative_to(LOCAL_PROGRESS_DIR.resolve())
    except ValueError:
        return None
    if not run_dir.exists() or not run_dir.is_dir():
        return None
    return run_dir


def load_run_result(run_id: str) -> Optional[Dict]:
    run_dir = safe_run_dir(run_id)
    if run_dir is None:
        return None

    meta_file = run_dir / "params.json"
    metadata: Dict = {}
    if meta_file.exists():
        try:
            metadata = json.loads(meta_file.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            metadata = {}

    fmt = str(metadata.get("format", "csv")).lower()
    out_file = run_dir / f"resultado.{fmt}"
    if not out_file.exists():
        alt_csv = run_dir / "resultado.csv"
        alt_json = run_dir / "resultado.json"
        alt_xlsx = run_dir / "resultado.xlsx"
        if alt_csv.exists():
            out_file = alt_csv
        elif alt_json.exists():
            out_file = alt_json
        else:
            out_file = alt_xlsx
        fmt = out_file.suffix.lstrip(".").lower() if out_file.exists() else "csv"

    columns, preview_rows = read_preview(out_file, fmt=fmt)
    log_file = run_dir / "execucao.log"
    row_count = None
    if log_file.exists():
        row_count = parse_record_count(log_file.read_text(encoding="utf-8", errors="replace"))

    return {
        "run_id": run_dir.name,
        "run_dir": str(run_dir),
        "success": int(metadata.get("exit_code", 1)) == 0 if metadata else False,
        "exit_code": int(metadata.get("exit_code", 1)) if metadata else 1,
        "row_count": row_count,
        "columns": columns,
        "preview_rows": preview_rows,
        "out_file_name": out_file.name if out_file.exists() else "",
        "log_file_name": "execucao.log" if log_file.exists() else "",
        "meta_file_name": "params.json" if meta_file.exists() else "",
        "log_tail": read_log_tail(log_file),
        "metadata": metadata,
    }


def list_recent_runs(limit: int = 25) -> List[Dict]:
    LOCAL_PROGRESS_DIR.mkdir(parents=True, exist_ok=True)
    runs: List[Dict] = []
    for path in sorted(LOCAL_PROGRESS_DIR.iterdir(), key=lambda p: p.name, reverse=True):
        if not path.is_dir() or path.name == ".gitkeep":
            continue
        meta_file = path / "params.json"
        log_file = path / "execucao.log"
        out_csv = path / "resultado.csv"
        out_json = path / "resultado.json"
        out_xlsx = path / "resultado.xlsx"
        fmt = "csv" if out_csv.exists() else "json" if out_json.exists() else "xlsx" if out_xlsx.exists() else ""
        record_count = None
        if log_file.exists():
            record_count = parse_record_count(log_file.read_text(encoding="utf-8", errors="replace"))
        metadata = {}
        if meta_file.exists():
            try:
                metadata = json.loads(meta_file.read_text(encoding="utf-8"))
            except json.JSONDecodeError:
                metadata = {}
        runs.append(
            {
                "run_id": path.name,
                "site": metadata.get("site", "-"),
                "url": metadata.get("url", "-"),
                "exit_code": metadata.get("exit_code", "-"),
                "format": metadata.get("format", fmt or "-"),
                "record_count": record_count,
            }
        )
        if len(runs) >= limit:
            break
    return runs


@app.get("/")
def index():
    run_id = normalize(request.args.get("run", ""))
    result = load_run_result(run_id) if run_id else None
    return render_template("index.html", form=DEFAULT_FORM, result=result, runs=list_recent_runs())


@app.post("/run")
def run_scrape():
    form_data = parse_form(request.form)
    errors = validate_form(form_data)
    if errors:
        for err in errors:
            flash(err, "error")
        return render_template("index.html", form=form_data, result=None, runs=list_recent_runs()), 400

    result = execute_scrape(form_data)
    if not result["success"]:
        flash("Execucao finalizou com erro. Veja o log abaixo.", "error")
    else:
        flash("Execucao concluida com sucesso.", "success")
    return render_template("index.html", form=form_data, result=result, runs=list_recent_runs())


@app.get("/run/<run_id>")
def open_run(run_id: str):
    result = load_run_result(run_id)
    if result is None:
        flash("Execucao nao encontrada.", "error")
        return redirect(url_for("index"))
    return render_template("index.html", form=DEFAULT_FORM, result=result, runs=list_recent_runs())


@app.get("/download/<run_id>/<filename>")
def download_file(run_id: str, filename: str):
    allowed = {"resultado.csv", "resultado.json", "resultado.xlsx", "execucao.log", "params.json"}
    if filename not in allowed:
        flash("Arquivo nao permitido para download.", "error")
        return redirect(url_for("index"))

    run_dir = safe_run_dir(run_id)
    if run_dir is None:
        flash("Execucao nao encontrada.", "error")
        return redirect(url_for("index"))

    file_path = run_dir / filename
    if not file_path.exists():
        flash("Arquivo nao encontrado.", "error")
        return redirect(url_for("open_run", run_id=run_id))

    return send_from_directory(run_dir, filename, as_attachment=True)


def main():
    parser = argparse.ArgumentParser(description="Painel web do Leiloes Scraper")
    parser.add_argument("--host", default="127.0.0.1", help="Host para bind do servidor")
    parser.add_argument("--port", default=8787, type=int, help="Porta do servidor")
    parser.add_argument("--debug", action="store_true", help="Liga debug do Flask")
    args = parser.parse_args()
    app.run(host=args.host, port=args.port, debug=args.debug)


if __name__ == "__main__":
    main()
