"""Builds a static site (one page per year, inline SVG charts) from BenchmarkDotNet JSON results.

usage: benchmark_site.py <results-dir> <output-dir>
"""
import collections
import datetime
import glob
import html
import json
import math
import re
import sys

NAME_RE = re.compile(r"^Run(\d{4})_(\d+)_(.+)$")
PALETTE = ["#2a6fdb", "#e8833a", "#2fa36b", "#b455c9", "#c9a227", "#d6455d", "#4aa3b8", "#7a7a7a"]

STYLE = """
:root{--bg:#fff;--fg:#1f2328;--muted:#656d76;--line:#d0d7de;--bar-bg:#f6f8fa}
@media(prefers-color-scheme:dark){:root{--bg:#0d1117;--fg:#e6edf3;--muted:#8b949e;--line:#30363d;--bar-bg:#161b22}}
body{font:15px/1.5 system-ui,sans-serif;background:var(--bg);color:var(--fg);max-width:1000px;margin:0 auto;padding:16px}
a{color:#2a6fdb}table{border-collapse:collapse;width:100%;margin:1em 0}
th,td{padding:4px 10px;border-bottom:1px solid var(--line);text-align:right}
th:first-child,td:first-child,th:nth-child(2),td:nth-child(2){text-align:left}
.muted{color:var(--muted)}svg{max-width:100%;height:auto}svg text{fill:var(--fg);font-size:12px}
.legend{display:flex;gap:16px;flex-wrap:wrap;margin:8px 0}.legend i{display:inline-block;width:12px;height:12px;margin-right:6px;border-radius:2px}
nav a{margin-right:12px}
"""


def fmt_time(ns):
    for unit, scale in (("s", 1e9), ("ms", 1e6), ("μs", 1e3)):
        if ns >= scale:
            return f"{ns / scale:,.2f} {unit}"
    return f"{ns:,.0f} ns"


def fmt_bytes(b):
    for unit, scale in (("MB", 1 << 20), ("KB", 1 << 10)):
        if b >= scale:
            return f"{b / scale:,.1f} {unit}"
    return f"{b:,.0f} B"


def load(results_dir):
    rows = []
    for path in glob.glob(f"{results_dir}/**/*-report-full*.json", recursive=True) + \
            glob.glob(f"{results_dir}/**/*-report.json", recursive=True):
        with open(path, encoding="utf-8") as f:
            data = json.load(f)
        for b in data["Benchmarks"]:
            m = NAME_RE.match(b["Method"])
            if not m or not b.get("Statistics"):
                continue
            rows.append({
                "year": int(m[1]), "day": int(m[2]), "name": m[3],
                "mean": b["Statistics"]["Mean"],
                "stddev": b["Statistics"].get("StandardDeviation", 0),
                "alloc": (b.get("Memory") or {}).get("BytesAllocatedPerOperation", 0),
            })
    return rows


def chart(day_rows, names):
    """Grouped horizontal bars per day on a log time axis."""
    row_h, group_gap, left, width, right = 16, 10, 50, 900, 90
    plot_w = width - left - right
    values = [r["mean"] for rs in day_rows.values() for r in rs]
    lo = 10 ** math.floor(math.log10(min(values)))
    hi = 10 ** math.ceil(math.log10(max(values)))
    span = math.log10(hi / lo) or 1

    def x(v):
        return left + plot_w * math.log10(max(v, lo) / lo) / span

    y = 24
    parts = []
    for day, rs in sorted(day_rows.items()):
        parts.append(f'<text x="{left - 8}" y="{y + len(rs) * row_h / 2 + 4}" text-anchor="end">Day {day}</text>')
        for r in sorted(rs, key=lambda r: names.index(r["name"])):
            w = max(x(r["mean"]) - left, 1)
            color = PALETTE[names.index(r["name"]) % len(PALETTE)]
            parts.append(
                f'<rect x="{left}" y="{y}" width="{w:.1f}" height="{row_h - 3}" fill="{color}" rx="2">'
                f'<title>Day {day} {html.escape(r["name"])}: {fmt_time(r["mean"])}</title></rect>'
                f'<text x="{left + w + 5:.1f}" y="{y + row_h - 5}">{fmt_time(r["mean"])}</text>')
            y += row_h
        y += group_gap
    height = y + 4
    axis = []
    exp = int(math.log10(lo))
    for e in range(exp, exp + int(round(span)) + 1):
        gx = x(10 ** e)
        axis.append(f'<line x1="{gx:.1f}" x2="{gx:.1f}" y1="16" y2="{height - 4}" stroke="var(--line)"/>'
                    f'<text x="{gx:.1f}" y="11" text-anchor="middle">{fmt_time(10 ** e)}</text>')
    return (f'<svg viewBox="0 0 {width} {height}" role="img" aria-label="Benchmark mean time per day (log scale)">'
            + "".join(axis) + "".join(parts) + "</svg>")


def page(title, body, years):
    nav = '<nav><a href="index.html">All years</a>' + "".join(
        f'<a href="{y}.html">{y}</a>' for y in years) + "</nav>"
    return (f'<!doctype html><html lang="en"><head><meta charset="utf-8">'
            f'<meta name="viewport" content="width=device-width,initial-scale=1">'
            f"<title>{html.escape(title)}</title><style>{STYLE}</style></head><body>"
            f"{nav}<h1>{html.escape(title)}</h1>{body}</body></html>")


def year_page(year, rows, years, stamp):
    names = sorted({r["name"] for r in rows})
    by_day = collections.defaultdict(list)
    for r in rows:
        by_day[r["day"]].append(r)
    legend = '<div class="legend">' + "".join(
        f'<span><i style="background:{PALETTE[i % len(PALETTE)]}"></i>{html.escape(n)}</span>'
        for i, n in enumerate(names)) + "</div>"
    table = ["<table><tr><th>Day</th><th>Method</th><th>Mean</th><th>Std dev</th><th>Allocated</th></tr>"]
    for day in sorted(by_day):
        for r in sorted(by_day[day], key=lambda r: r["mean"]):
            table.append(f"<tr><td>{day}</td><td>{html.escape(r['name'])}</td><td>{fmt_time(r['mean'])}</td>"
                         f"<td>{fmt_time(r['stddev'])}</td><td>{fmt_bytes(r['alloc'])}</td></tr>")
    table.append("</table>")
    total = sum(min(r["mean"] for r in rs) for rs in by_day.values())
    body = (f'<p class="muted">Mean time per day on a log scale. Fastest solution per day totals '
            f"<b>{fmt_time(total)}</b> across {len(by_day)} days. Generated {stamp}.</p>"
            + legend + chart(by_day, names) + "".join(table))
    return page(f"Advent of Code {year} benchmarks", body, years)


def main():
    results_dir, out_dir = sys.argv[1:3]
    rows = load(results_dir)
    if not rows:
        sys.exit("no benchmark results found")
    by_year = collections.defaultdict(list)
    for r in rows:
        by_year[r["year"]].append(r)
    years = sorted(by_year)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    import os
    os.makedirs(out_dir, exist_ok=True)
    summary = ["<table><tr><th>Year</th><th>Days</th><th>Fastest total</th></tr>"]
    for y in years:
        by_day = collections.defaultdict(list)
        for r in by_year[y]:
            by_day[r["day"]].append(r["mean"])
        summary.append(f'<tr><td><a href="{y}.html">{y}</a></td><td>{len(by_day)}</td>'
                       f"<td>{fmt_time(sum(min(v) for v in by_day.values()))}</td></tr>")
        with open(f"{out_dir}/{y}.html", "w", encoding="utf-8") as f:
            f.write(year_page(y, by_year[y], years, stamp))
    summary.append("</table>")
    with open(f"{out_dir}/index.html", "w", encoding="utf-8") as f:
        f.write(page("Advent of Code benchmarks",
                     f'<p class="muted">Generated {stamp} on a GitHub Actions runner; '
                     f"absolute times vary between runs, compare within a page.</p>" + "".join(summary), years))


if __name__ == "__main__":
    main()
