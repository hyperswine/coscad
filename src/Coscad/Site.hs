-- | `coscad site DIR spec.assemble ...`: a static, phone-first companion
-- site. One page per build (steps with images, preload sheet, BOM,
-- step-through navigation, in-build search) and an index with search
-- across builds. Plain HTML/CSS/JS, no server, host anywhere.
module Coscad.Site (processSite, buildPageHtml, indexHtml) where

import Control.Monad (forM, forM_, unless)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Plan (processPlan)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- | Crude JSON string escaping for values we embed into <script> blocks.
jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '<' = "\\u003c"
    esc '\n' = "\\n"
    esc c = [c]

-- | The build page: everything is rendered client-side from the embedded
-- plan JSON, so the same page works from a file:// URL or a web host.
buildPageHtml :: String -> String -> String -> Int -> String
buildPageHtml name planJson imagePrefix nSteps = unlines
  [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" ++ name ++ " build</title>"
  , "<style>" ++ css ++ "</style></head><body>"
  , "<header><a class=\"back\" href=\"../index.html\">&larr; builds</a><h1>" ++ name ++ "</h1><div id=\"summary\" class=\"muted\"></div>"
  , "<input id=\"q\" type=\"search\" placeholder=\"filter steps: part, rail, screw&hellip;\" autocomplete=\"off\"></header>"
  , "<main><details id=\"before\"><summary>Before you start</summary><div id=\"bom\"></div><div id=\"preload\"></div></details>"
  , "<div id=\"errors\"></div><ol id=\"steps\"></ol></main>"
  , "<nav id=\"bar\"><button id=\"prev\">&#9664; Prev</button><span id=\"pos\">&nbsp;</span><button id=\"next\">Next &#9654;</button></nav>"
  , "<script>const PLAN = " ++ planJson ++ ";\nconst IMG = " ++ jstr imagePrefix ++ ";\nconst NSTEPS = " ++ show nSteps ++ ";"
  , js ++ "</script></body></html>"
  ]

css :: String
css = unwords
  [ ":root{--bg:#f4f3ef;--card:#fffefb;--ink:#1f1e1a;--muted:#6f6c62;--accent:#b8541c;--line:#e2dfd6;color-scheme:light}"
  , "@media(prefers-color-scheme:dark){:root:not([data-theme=light]){--bg:#161512;--card:#211f1b;--ink:#ebe8e0;--muted:#9d998c;--accent:#e6823f;--line:#33302a;color-scheme:dark}}"
  , ":root[data-theme=dark]{--bg:#161512;--card:#211f1b;--ink:#ebe8e0;--muted:#9d998c;--accent:#e6823f;--line:#33302a;color-scheme:dark}"
  , "*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.45 -apple-system,system-ui,Segoe UI,Roboto,sans-serif;padding-bottom:72px}"
  , "header{padding:12px 16px 8px;position:sticky;top:env(safe-area-inset-top,0px);background:var(--bg);border-bottom:1px solid var(--line);z-index:2}"
  , "h1{margin:2px 0 4px;font-size:22px;text-wrap:balance;font-variant-numeric:tabular-nums}.muted{color:var(--muted);font-size:14px}.back{color:var(--muted);text-decoration:none;font-size:14px}"
  , "input[type=search]{width:100%;margin-top:8px;padding:10px 12px;border:1px solid var(--line);border-radius:10px;background:var(--card);color:var(--ink);font-size:16px}"
  , "main{padding:12px 12px 24px;max-width:760px;margin:0 auto}details{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:10px 14px;margin-bottom:12px}summary{font-weight:600;cursor:pointer}"
  , "ol#steps{list-style:none;padding:0;margin:0}li.step{background:var(--card);border:1px solid var(--line);border-radius:14px;margin:0 0 14px;overflow:hidden;scroll-margin-top:120px}"
  , "li.step.current{border-color:var(--accent);box-shadow:0 0 0 2px var(--accent)}li.step img{display:block;width:100%;height:auto;background:#fafafa}"
  , ".body{padding:10px 14px 14px}.title{display:flex;gap:8px;align-items:baseline;flex-wrap:wrap;margin-bottom:6px}.title b{font-size:18px}"
  , ".badge{font-size:12px;padding:2px 8px;border-radius:999px;background:var(--line)}.badge.flip{background:var(--accent);color:#fff}"
  , "ul{margin:4px 0 0;padding-left:18px}li.warn{color:var(--accent)}.torque{font-weight:600}code{background:var(--line);padding:0 4px;border-radius:4px}"
  , "nav#bar{position:fixed;bottom:0;left:0;right:0;display:flex;gap:8px;align-items:center;justify-content:space-between;padding:10px 12px calc(10px + env(safe-area-inset-bottom));background:var(--card);border-top:1px solid var(--line)}"
  , "nav#bar button{flex:1;padding:12px;font-size:16px;border-radius:10px;border:1px solid var(--line);background:var(--bg);color:var(--ink)}nav#bar span{white-space:nowrap;color:var(--muted)}"
  , ".err{background:var(--card);border:1px solid var(--accent);color:var(--accent);border-radius:10px;padding:10px 14px;margin-bottom:12px}"
  , ".hidden{display:none}"
  ]

js :: String
js = unlines
  [ "const $ = s => document.querySelector(s);"
  , "const esc = s => String(s).replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));"
  , "const steps = PLAN.steps;"
  , "$('#summary').textContent = `${new Set(steps.flatMap(s => s.parts)).size} parts · ${steps.reduce((n, s) => n + s.fasteners.length, 0)} screws · ${steps.length} steps · ${steps.filter(s => s.flip).length} flips`;"
  , "if (PLAN.design_errors.length) $('#errors').innerHTML = PLAN.design_errors.map(e => `<div class=err>Design error: ${esc(e)}</div>`).join('');"
  , "const bom = {}; for (const s of steps) for (const f of s.fasteners) { const k = `${f.spec} ${f.head} head screw + M${f.spec.replace(/^M(\\d+(?:\\.\\d+)?).*/i, '$1')} ${f.nut === 'DropIn' ? 'drop-in' : 'slide-in'} T-nut`; bom[k] = (bom[k] || 0) + 1; }"
  , "$('#bom').innerHTML = '<b>Bill of materials</b><ul>' + Object.entries(bom).map(([k, n]) => `<li>${n} × ${esc(k)}</li>`).join('') + '</ul>';"
  , "const pre = {}; for (const s of steps) for (const f of s.fasteners) { (pre[f.host] = pre[f.host] || []).push(`${f.face} @ ${f.at_mm} mm`); }"
  , "$('#preload').innerHTML = '<b>T-nut preload sheet</b> <span class=muted>(mark the rail\\'s low end; positions from the mark)</span><ul>' + Object.entries(pre).map(([h, ps]) => `<li><b>${esc(h)}</b>: ${ps.length} nut${ps.length > 1 ? 's' : ''} — ${ps.map(esc).join(', ')}</li>`).join('') + '</ul>';"
  , "const torqueWords = f => f.torque_nm < 1 ? `${f.torque_nm} Nm (printed lips: finger tight + ¼ turn, no power driver)` : `${f.torque_nm} Nm`;"
  , "$('#steps').innerHTML = steps.map((s, i) => `<li class=step id=step-${i + 1} data-text=\"${esc([s.rest, ...s.parts, ...s.preload, ...s.fasteners.flatMap(f => [f.spec, f.clamped, f.host, f.face])].join(' ').toLowerCase())}\">"
  , "  <img loading=lazy src=\"${IMG}${i + 1}.png\" alt=\"step ${i + 1}\" onerror=\"this.style.display='none'\">"
  , "  <div class=body><div class=title><b>Step ${i + 1}</b><span class=badge>${esc(s.rest)} down</span>${s.flip ? '<span class=\"badge flip\">flip the assembly</span>' : ''}</div><ul>"
  , "  ${s.parts.map(p => `<li>Place <b>${esc(p)}</b></li>`).join('')}"
  , "  ${s.preload.map(h => `<li>Preload <b>${esc(h)}</b>: ${(pre[h] || []).length} T-nut${(pre[h] || []).length > 1 ? 's' : ''} (${(pre[h] || []).map(esc).join(', ')})</li>`).join('')}"
  , "  ${s.fasteners.map(f => `<li>Tighten <code>${esc(f.spec)}</code> ${esc(f.head)} head through <b>${esc(f.clamped)}</b> into <b>${esc(f.host)}</b> ${esc(f.face)} slot at ${f.at_mm} mm — <span class=torque>${esc(torqueWords(f))}</span></li>`).join('')}"
  , "  ${s.warnings.map(w => `<li class=warn>${esc(w)}</li>`).join('')}</ul></div></li>`).join('');"
  , "let cur = Math.max(1, Math.min(steps.length, parseInt(location.hash.replace('#step-', '')) || 1));"
  , "function show(n, scroll = true) { cur = Math.max(1, Math.min(steps.length, n)); document.querySelectorAll('li.step').forEach(li => li.classList.remove('current')); const li = $('#step-' + cur); if (li) { li.classList.add('current'); if (scroll) li.scrollIntoView({ behavior: 'smooth', block: 'start' }); } $('#pos').textContent = `${cur} / ${steps.length}`; history.replaceState(null, '', '#step-' + cur); }"
  , "$('#prev').onclick = () => show(cur - 1); $('#next').onclick = () => show(cur + 1);"
  , "document.addEventListener('keydown', e => { if (e.key === 'ArrowRight' || e.key === 'j') show(cur + 1); if (e.key === 'ArrowLeft' || e.key === 'k') show(cur - 1); });"
  , "$('#q').addEventListener('input', e => { const q = e.target.value.trim().toLowerCase(); document.querySelectorAll('li.step').forEach(li => li.classList.toggle('hidden', q && !li.dataset.text.includes(q))); });"
  , "new IntersectionObserver(es => { for (const en of es) if (en.isIntersecting) { const n = parseInt(en.target.id.replace('step-', '')); if (n !== cur) show(n, false); } }, { rootMargin: '-45% 0px -45% 0px' }).observe && document.querySelectorAll('li.step').forEach(li => new IntersectionObserver(es => { for (const en of es) if (en.isIntersecting) { const n = parseInt(en.target.id.replace('step-', '')); if (n !== cur) { cur = n; document.querySelectorAll('li.step').forEach(x => x.classList.remove('current')); en.target.classList.add('current'); $('#pos').textContent = `${cur} / ${steps.length}`; history.replaceState(null, '', '#step-' + cur); } } }, { rootMargin: '-45% 0px -45% 0px' }).observe(li));"
  , "show(cur, false);"
  ]

-- | The index: search across builds by name, part, material, fastener spec.
indexHtml :: [(String, String, [String], Int, Int, Int)] -> String
indexHtml builds = unlines
  [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>CoScad builds</title>"
  , "<style>" ++ css ++ " .card{display:block;background:var(--card);border:1px solid var(--line);border-radius:14px;margin-bottom:12px;overflow:hidden;color:inherit;text-decoration:none}.card img{width:100%;display:block;background:#fafafa}.card .body b{font-size:18px}</style></head><body>"
  , "<header><h1>Builds</h1><div class=muted>" ++ show (length builds) ++ " assembl" ++ (if length builds == 1 then "y" else "ies") ++ "</div><input id=\"q\" type=\"search\" placeholder=\"search builds: name, part, screw&hellip;\" autocomplete=\"off\"></header>"
  , "<main id=\"list\"></main>"
  , "<script>const BUILDS = [" ++ intercalate "," [ "{name:" ++ jstr n ++ ",dir:" ++ jstr d ++ ",tags:" ++ jstr (unwords tags) ++ ",parts:" ++ show p ++ ",screws:" ++ show f ++ ",steps:" ++ show s ++ "}" | (n, d, tags, p, f, s) <- builds ] ++ "];"
  , "const esc = s => String(s).replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));"
  , "function render(q) { document.getElementById('list').innerHTML = BUILDS.filter(b => !q || (b.name + ' ' + b.tags).toLowerCase().includes(q)).map(b => `<a class=card href=\"${esc(b.dir)}/index.html\"><img loading=lazy src=\"${esc(b.dir)}/${esc(b.name)}_step1.png\" alt=\"\" onerror=\"this.style.display='none'\"><div class=body><b>${esc(b.name)}</b><div class=muted>${b.parts} parts · ${b.screws} screws · ${b.steps} steps</div><div class=muted>${esc(b.tags)}</div></div></a>`).join('') || '<div class=muted>no builds match</div>'; }"
  , "document.getElementById('q').addEventListener('input', e => render(e.target.value.trim().toLowerCase())); render('');"
  , "</script></body></html>"
  ]

-- | Plan every assembly (with PNG renders), collect the outputs under
-- DIR/<name>/, and write the index.
processSite :: FilePath -> [FilePath] -> IO ()
processSite outDir specs = do
  createDirectoryIfMissing True outDir
  builds <- forM specs $ \spec -> do
    processPlan True spec
    let base = dropExtension spec
        name = takeFileName base
        srcDir = takeDirectory spec
        dest = outDir </> name
    createDirectoryIfMissing True dest
    files <- listDirectory srcDir
    let mine = [f | f <- files, (name ++ "_step") `isPrefixOf` f && ".png" `isSuffixOf` f]
          ++ [name ++ "_plan.md", name ++ "_plan.json"]
    forM_ mine $ \f -> do
      ok <- doesFileExist (srcDir </> f)
      if ok then copyFile (srcDir </> f) (dest </> f) else return ()
    planJson <- readFileUtf8 (base ++ "_plan.json")
    let nSteps = length [f | f <- files, (name ++ "_step") `isPrefixOf` f && ".png" `isSuffixOf` f]
        page = buildPageHtml name (trim planJson) (name ++ "_step") nSteps
    writeFileUtf8 (dest </> "index.html") page
    spec' <- readFileUtf8 spec
    let tags = sort (uniq [w | l <- lines spec', let ws = words l, take 1 ws /= ["//"], w <- ws, any (`isPrefixOf` w) ["material=", "profile=", "M"], length w > 1])
        count key = length (filter (\l -> (key ++ " ") `isPrefixOf` l || (key ++ "\t") `isPrefixOf` l) (lines spec'))
        parts = length (filter ('←' `elem`) (lines spec'))
    putStrLn ("Wrote " ++ dest </> "index.html")
    return (name, name, tags, parts, count "fastener", nSteps)
  writeFileUtf8 (outDir </> "index.html") (indexHtml builds)
  putStrLn ("Wrote " ++ outDir </> "index.html" ++ " (" ++ show (length builds) ++ " builds)")
  unless (not (null builds)) $ hPutStrLn stderr "Error: no assemblies given" >> exitFailure
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
