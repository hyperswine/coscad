// Run after build-manual.sh: node scripts/test-manual-layout.cjs OUTDIR
// Requires Playwright; optionally set CHROMIUM_EXECUTABLE for an existing browser.
const fs = require('fs'), path = require('path'), assert = require('assert/strict');
const { pathToFileURL } = require('url');
const { chromium } = require('playwright');

async function checkLayout(page, file, width) {
  const layout = await page.evaluate(() => ({
    width: document.documentElement.scrollWidth,
    nav: document.querySelectorAll('nav[aria-label="Main navigation"]').length,
    overflow: [...document.querySelectorAll('main,li.step,.body,.card,header')]
      .filter(e => { const r = e.getBoundingClientRect(); return r.left < -.5 || r.right > innerWidth + .5; })
      .map(e => e.tagName + '.' + e.className)
  }));
  assert(layout.width <= width, `${file} at ${width}: document width ${layout.width}`);
  assert.equal(layout.nav, 1, file);
  assert.deepEqual(layout.overflow, [], `${file} at ${width}`);
}

async function run() {
  assert(process.argv[2], 'Pass the generated manual directory');
  const root = path.resolve(process.argv[2]);
  const browser = await chromium.launch(process.env.CHROMIUM_EXECUTABLE ? {executablePath:process.env.CHROMIUM_EXECUTABLE} : {});
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', e => errors.push(e.message));
    const files = fs.readdirSync(root).filter(f => f.endsWith('.html')).map(f => path.join(root,f));
    files.push(path.join(root,'builds/index.html'));
    for (const d of fs.readdirSync(path.join(root,'builds'), {withFileTypes:true}).filter(d => d.isDirectory()))
      files.push(path.join(root,'builds',d.name,'index.html'));
    let count = 0;
    for (const width of [320,375,768,1440]) {
      await page.setViewportSize({width,height:800});
      for (const file of files) {
        await page.goto(pathToFileURL(file).href);
        await page.locator('main img').evaluateAll(images => Promise.all(images.map(image => new Promise((resolve,reject) => {
          image.loading = 'eager';
          const done = () => image.naturalWidth ? resolve() : reject(new Error('Missing instruction image: ' + image.src));
          if (image.complete) done(); else { image.onload = done; image.onerror = done; }
        }))));
        await checkLayout(page,file,width);
        // A long unbroken component name must wrap inside each card or step.
        await page.evaluate(() => {
          document.querySelectorAll('.card .body,li.step .body').forEach(e => {
            const stress = document.createElement('b'); stress.textContent = 'component_'.repeat(80); e.append(stress);
          });
        });
        await checkLayout(page,file,width);
        await page.locator('.nav-menu summary').click();
        const menu = await page.locator('.nav-links').boundingBox();
        assert(menu.x >= 0 && menu.x + menu.width <= width + .5, file + ' menu overflow');
        await page.locator('.nav-menu summary').click();
        await page.evaluate(() => scrollTo(0,document.body.scrollHeight));
        const nav = await page.locator('nav.top').boundingBox();
        assert(nav.y >= -.5 && nav.y < 1, file + ' sticky nav');
        count++;
      }
    }
    await page.goto(pathToFileURL(path.join(root,'builds/orbit-tower/index.html')).href);
    await page.locator('#q').focus();
    await page.keyboard.type('j');
    assert.equal(await page.locator('#q').inputValue(),'j');
    assert.deepEqual(errors,[],'Browser script errors');
    console.log(`${count} page/viewport checks passed, including long component names and sticky menus`);
  } finally { await browser.close(); }
}
run().catch(e => { console.error(e); process.exitCode=1; });
