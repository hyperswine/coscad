-- | `coscad site DIR spec.assemble ...`: a static, phone-first companion
-- site. One page per build (steps with images, preload sheet, BOM,
-- step-through navigation, in-build search) and an index with search
-- across builds. Plain HTML/CSS/JS, no server, host anywhere.
module Coscad.Site (processSite, buildPageHtml, indexHtml) where

import Control.Monad (forM, forM_, unless)
import Coscad.IO (readFileUtf8, writeFileUtf8)
import Coscad.Plan (PlanSummary (..), StepSummary (..), processPlanSummary)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import Coscad.Json
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- | Crude JSON string escaping for values we embed into <script> blocks.
jstr :: String -> String
jstr = encode . JString

-- | JSON inside a <script> element must not contain "</", or the browser
-- ends the script there.
scriptSafe :: String -> String
scriptSafe [] = []
scriptSafe ('<' : '/' : r) = "<\\/" ++ scriptSafe r
scriptSafe (c : r) = c : scriptSafe r

-- | The build page: everything is rendered client-side from the embedded
-- plan JSON, so the same page works from a file:// URL or a web host.
buildPageHtml :: String -> String -> String -> Int -> String
buildPageHtml name planJson imagePrefix nSteps = unlines
  [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>" ++ name ++ " build</title>"
  , "<style>" ++ css ++ "</style></head><body>"
  , siteNav "../"
  , "<header><a class=\"back\" href=\"../index.html\">&larr; builds</a><h1>" ++ name ++ "</h1><div id=\"summary\" class=\"muted\"></div>"
  , "<input id=\"q\" type=\"search\" placeholder=\"filter steps: part, rail, joint&hellip;\" autocomplete=\"off\">"
  , "<div class=\"muted\">Generated instructions · <a href=\"" ++ name ++ "_plan.md\">Markdown</a> · <a href=\"" ++ name ++ "_plan.json\">JSON</a></div></header>"
  , "<main><details id=\"before\"><summary>Before you start</summary><div id=\"bom\"></div><div id=\"preload\"></div></details>"
  , "<div id=\"errors\"></div><ol id=\"steps\"></ol></main>"
  , "<nav id=\"bar\"><button id=\"prev\">&#9664; Prev</button><span id=\"pos\">&nbsp;</span><button id=\"next\">Next &#9654;</button></nav>"
  , "<script>const PLAN = " ++ scriptSafe planJson ++ ";\nconst IMG = " ++ jstr imagePrefix ++ ";\nconst NSTEPS = " ++ show nSteps ++ ";"
  , js ++ "</script></body></html>"
  ]

-- Standalone builds keep local navigation. The manual builder replaces this
-- with the shared documentation navigation when publishing the full manual.
siteNav :: String -> String
siteNav prefix = "<nav class=\"top\" aria-label=\"Main navigation\"><a class=\"brand\" href=\"" ++ prefix ++ "index.html\">CoScad builds</a><a href=\"" ++ prefix ++ "index.html\">All builds</a></nav>"

css :: String
css = unwords
  [ ":root{--bg:#f4f3ef;--card:#fffefb;--ink:#1f1e1a;--muted:#6f6c62;--accent:#b8541c;--line:#e2dfd6;color-scheme:light}"
  , "@media(prefers-color-scheme:dark){:root:not([data-theme=light]){--bg:#161512;--card:#211f1b;--ink:#ebe8e0;--muted:#9d998c;--accent:#e6823f;--line:#33302a;color-scheme:dark}}"
  , ":root[data-theme=dark]{--bg:#161512;--card:#211f1b;--ink:#ebe8e0;--muted:#9d998c;--accent:#e6823f;--line:#33302a;color-scheme:dark}"
  , "*{box-sizing:border-box}body{margin:0;background:var(--bg);color:var(--ink);font:16px/1.45 -apple-system,system-ui,Segoe UI,Roboto,sans-serif;padding-bottom:72px}"
  , "a{color:var(--accent)}"
  , "header{width:100%;max-width:860px;margin:0 auto;padding:12px 16px 8px;background:var(--bg);border-bottom:1px solid var(--line)}"
  , "h1{margin:2px 0 4px;font-size:22px;text-wrap:balance;font-variant-numeric:tabular-nums}.muted{color:var(--muted);font-size:14px}.back{color:var(--muted);text-decoration:none;font-size:14px}"
  , "input[type=search]{width:100%;margin-top:8px;padding:10px 12px;border:1px solid var(--line);border-radius:10px;background:var(--card);color:var(--ink);font-size:16px}"
  , "main{width:100%;min-width:0;padding:12px 16px 24px;max-width:860px;margin:0 auto}details{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:10px 14px;margin-bottom:12px}summary{font-weight:600;cursor:pointer}"
  , "ol#steps{list-style:none;padding:0;margin:0}li.step{background:var(--card);border:1px solid var(--line);border-radius:14px;margin:0 0 14px;overflow:hidden;scroll-margin-top:calc(var(--nav-height,64px) + 16px)}"
  , "li.step.current{border-color:var(--accent);box-shadow:0 0 0 2px var(--accent)}li.step img{display:block;width:100%;height:auto;background:#fafafa}"
  , ".body{padding:10px 14px 14px}.title{display:flex;gap:8px;align-items:baseline;flex-wrap:wrap;margin-bottom:6px}.title b{font-size:18px}"
  , ".badge{font-size:12px;padding:2px 8px;border-radius:999px;background:var(--line)}.badge.flip{background:var(--accent);color:#fff}"
  , "ul{margin:4px 0 0;padding-left:18px}li.warn{color:var(--accent)}.torque{font-weight:600}code{background:var(--line);padding:0 4px;border-radius:4px}"
  , "nav#bar{max-width:860px;margin:0 auto;position:fixed;bottom:0;left:0;right:0;display:flex;gap:8px;align-items:center;justify-content:space-between;padding:10px 12px calc(10px + env(safe-area-inset-bottom));background:var(--card);border-top:1px solid var(--line)}"
  , "nav#bar button{min-width:0;flex:1;padding:12px;font-size:16px;border-radius:10px;border:1px solid var(--line);background:var(--bg);color:var(--ink)}nav#bar span{white-space:nowrap;color:var(--muted)}"
  , ".err{background:var(--card);border:1px solid var(--accent);color:var(--accent);border-radius:10px;padding:10px 14px;margin-bottom:12px}"
  , ".hidden{display:none}"
  , "body{overflow-wrap:anywhere}main>*,.body,.title,.card{min-width:0;max-width:100%}input{min-width:0}img{max-width:100%;height:auto}pre{max-width:100%;overflow:auto}"
  , "nav.top{position:sticky;top:0;z-index:10;display:flex;flex-wrap:wrap;gap:12px;padding:calc(12px + env(safe-area-inset-top,0px)) 16px 12px;background:var(--card);border-bottom:1px solid var(--line)}nav.top a{text-decoration:none}nav.top .brand{font-weight:700}"
  ]

js :: String
js = unlines
  [ "const $ = s => document.querySelector(s);"
  , "const esc = s => String(s).replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));"
  , "const steps = PLAN.steps;"
  , "const isPeg = f => f.spec.toLowerCase() === 'peg';"
  , "const fasteners = steps.flatMap(s => s.fasteners), screws = fasteners.filter(f => !isPeg(f));"
  , "$('#summary').textContent = `${new Set(steps.flatMap(s => s.parts)).size} parts · ${fasteners.filter(isPeg).length} peg joints · ${screws.length} screws · ${steps.length} steps · ${steps.filter(s => s.flip).length} flips`;"
  , "if (PLAN.design_errors.length) $('#errors').innerHTML = PLAN.design_errors.map(e => `<div class=err>Design error: ${esc(e)}</div>`).join('');"
  , "const nutWords = f => f.nut === 'hex' ? 'hex nut' : f.nut === 'dropin' ? 'drop-in T-nut' : 'slide-in T-nut';"
  , "const diameter = f => f.spec.replace(/^M(\\d+(?:\\.\\d+)?).*/i, '$1');"
  , "const bom = {}, addBom = k => { bom[k] = (bom[k] || 0) + 1; };"
  , "const parts = PLAN.parts || [...new Set(steps.flatMap(s => s.parts))].map(name => ({part: name.replace(/#\\d+$/, '')}));"
  , "for (const p of parts) addBom(p.part + (p.material ? ` (${p.material})` : ''));"
  , "for (const f of screws) { addBom(`${f.spec} ${f.head} head screw`); if (f.nut !== 'none') addBom(`M${diameter(f)} ${nutWords(f)}`); }"
  , "$('#bom').innerHTML = '<b>Bill of materials</b><ul>' + Object.entries(bom).map(([k, n]) => `<li>${n} × ${esc(k)}</li>`).join('') + '</ul>';"
  , "const pocketLocation = p => { const ref = p.replace(/^(clamped|host)\\./, ''); return ref.startsWith('@') ? 'at ' + ref : 'on the ' + ref + ' face'; };"
  , "const holeLocation = f => f.face.startsWith('@') ? 'hole ' + f.face : f.face + ' face';"
  , "const preloadWords = f => f.nut === 'hex' ? `M${diameter(f)} hex nut${f.pocket ? ' into the pocket ' + pocketLocation(f.pocket) : ''}` : `M${diameter(f)} ${nutWords(f)}, ${f.face} slot @ ${f.at_mm} mm from the marked low end`;"
  , "const pre = {}; for (const f of fasteners) if (!isPeg(f) && f.nut !== 'none') { const h = (f.pocket || '').startsWith('clamped.') ? f.clamped : f.host; (pre[h] = pre[h] || []).push(preloadWords(f)); }"
  , "$('#preload').innerHTML = '<b>Nut preload sheet</b>' + (Object.keys(pre).length ? '<ul>' + Object.entries(pre).map(([h, ps]) => `<li><b>${esc(h)}</b>: ${ps.map(esc).join('; ')}</li>`).join('') + '</ul>' : '<p>No nuts to preload.</p>');"
  , "const torqueWords = f => f.torque_nm < 1 ? `${f.torque_nm} Nm (printed lips: finger tight + ¼ turn, no power driver)` : `${f.torque_nm} Nm`;"
  , "const jointWords = f => isPeg(f) ? `Fit <b>${esc(f.clamped)}</b> to <b>${esc(f.host)}</b> from the ${esc(f.face)} entry${f.inset_mm ? `, onto the seat ${f.inset_mm} mm below the face` : ''} until seated` : `Tighten <code>${esc(f.spec)}</code> ${esc(f.head)} head through <b>${esc(f.clamped)}</b> into <b>${esc(f.host)}</b> (${esc(holeLocation(f))}${f.nut === 'none' ? ', threads into the part' : f.nut === 'hex' ? ', captive hex nut' : ', slot @ ' + f.at_mm + ' mm'}) — <span class=torque>${esc(torqueWords(f))}</span>`;"
  , "$('#steps').innerHTML = steps.map((s, i) => `<li class=step id=step-${i + 1} data-text=\"${esc([s.rest, ...s.parts, ...s.preload, ...s.fasteners.flatMap(f => [f.spec, f.clamped, f.host, f.face])].join(' ').toLowerCase())}\">"
  , "  <img loading=lazy width=1200 height=900 src=\"${IMG}${i + 1}.png\" alt=\"step ${i + 1}\" onerror=\"this.style.display='none'\">"
  , "  <div class=body><div class=title><b>Step ${i + 1}</b><span class=badge>${esc(s.rest)} down</span>${s.flip ? '<span class=\"badge flip\">flip the assembly</span>' : ''}</div><ul>"
  , "  ${s.parts.map(p => `<li>Place <b>${esc(p)}</b></li>`).join('')}"
  , "  ${s.preload.map(h => `<li>Preload <b>${esc(h)}</b>: ${(pre[h] || []).map(esc).join('; ')}</li>`).join('')}"
  , "  ${s.fasteners.map(f => `<li>${jointWords(f)}</li>`).join('')}"
  , "  ${s.warnings.map(w => `<li class=warn>${esc(w)}</li>`).join('')}</ul></div></li>`).join('');"
  , "let cur = Math.max(1, Math.min(steps.length, parseInt(location.hash.replace('#step-', '')) || 1));"
  , "function show(n, scroll = true) { cur = Math.max(1, Math.min(steps.length, n)); document.querySelectorAll('li.step').forEach(li => li.classList.remove('current')); const li = $('#step-' + cur); if (li) { li.classList.add('current'); if (scroll) li.scrollIntoView({ behavior: 'smooth', block: 'start' }); } $('#pos').textContent = `${cur} / ${steps.length}`; history.replaceState(null, '', '#step-' + cur); }"
  , "$('#prev').onclick = () => show(cur - 1); $('#next').onclick = () => show(cur + 1);"
  , "document.addEventListener('keydown', e => { if (e.target.matches('input,textarea,select,[contenteditable]') || e.ctrlKey || e.metaKey || e.altKey) return; if (e.key === 'ArrowRight' || e.key === 'j') show(cur + 1); if (e.key === 'ArrowLeft' || e.key === 'k') show(cur - 1); });"
  , "$('#q').addEventListener('input', e => { const q = e.target.value.trim().toLowerCase(); document.querySelectorAll('li.step').forEach(li => li.classList.toggle('hidden', q && !li.dataset.text.includes(q))); });"
  , "if ('IntersectionObserver' in window) { const observer = new IntersectionObserver(es => { for (const en of es) if (en.isIntersecting) show(parseInt(en.target.id.replace('step-', '')), false); }, { rootMargin: '-45% 0px -45% 0px' }); document.querySelectorAll('li.step').forEach(li => observer.observe(li)); }"
  , "if ('ResizeObserver' in window) new ResizeObserver(es => document.documentElement.style.setProperty('--nav-height', es[0].target.offsetHeight + 'px')).observe(document.querySelector('nav.top'));"
  , "show(cur, false);"
  ]

-- | The index: search across builds by name, part, material, fastener spec.
indexHtml :: [(String, String, [String], Int, Int, Int)] -> String
indexHtml builds = unlines
  [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><title>CoScad builds</title>"
  , "<style>" ++ css ++ " .card{display:block;background:var(--card);border:1px solid var(--line);border-radius:14px;margin-bottom:12px;overflow:hidden;color:inherit;text-decoration:none}.card img{width:100%;display:block;background:#fafafa}.card .body b{font-size:18px}</style></head><body>"
  , siteNav ""
  , "<header><h1>Builds</h1><div class=muted>" ++ show (length builds) ++ " assembl" ++ (if length builds == 1 then "y" else "ies") ++ "</div><input id=\"q\" type=\"search\" placeholder=\"search builds: name, part, screw&hellip;\" autocomplete=\"off\"></header>"
  , "<main id=\"list\"></main>"
  , "<script>const BUILDS = " ++ scriptSafe (encode (jlist [jobj [("name", JString n), ("dir", JString d), ("tags", JString (unwords tags)), ("parts", JNumber (fromIntegral p)), ("screws", JNumber (fromIntegral f)), ("steps", JNumber (fromIntegral s))] | (n, d, tags, p, f, s) <- builds])) ++ ";"
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
    summary <- processPlanSummary True spec
    let base = dropExtension spec
        name = takeFileName base
        srcDir = takeDirectory spec
        dest = outDir </> name
        nSteps = length (psSteps summary)
    createDirectoryIfMissing True dest
    let mine = [name ++ "_step" ++ show k ++ ".png" | k <- [1 .. nSteps]]
          ++ [name ++ "_plan.md", name ++ "_plan.json"]
    forM_ mine $ \f -> do
      ok <- doesFileExist (srcDir </> f)
      if ok then copyFile (srcDir </> f) (dest </> f) else return ()
    planJson <- readFileUtf8 (base ++ "_plan.json")
    let page = buildPageHtml name (trim planJson) (name ++ "_step") nSteps
    writeFileUtf8 (dest </> "index.html") page
    spec' <- readFileUtf8 spec
    let tags = sort (uniq [w | l <- lines spec', let ws = words l, take 1 ws /= ["//"], w <- ws, any (`isPrefixOf` w) ["material=", "profile=", "M"], length w > 1])
        screws = length [() | s <- psSteps summary, (specName, _, _, _) <- ssFasteners s, specName /= "peg"]
    putStrLn ("Wrote " ++ dest </> "index.html")
    return (name, name, tags, length (psParts summary), screws, nSteps)
  writeFileUtf8 (outDir </> "index.html") (indexHtml builds)
  putStrLn ("Wrote " ++ outDir </> "index.html" ++ " (" ++ show (length builds) ++ " builds)")
  unless (not (null builds)) $ hPutStrLn stderr "Error: no assemblies given" >> exitFailure
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
