const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

const source = fs.readFileSync(path.join(__dirname, '../public/app-shell.20261028a.js'), 'utf8');
const settle = () => new Promise(resolve => setImmediate(resolve));

test('opening or focusing the app only reads status; timed retries use the open conversation and record', async () => {
  const calls = [];
  const windowEvents = new Map();
  const documentEvents = new Map();
  const timers = [];
  const window = {addEventListener: (event, fn) => windowEvents.set(event, fn)};
  window.parent = window;
  const location = new URL('https://app.test/analysis/');
  window.location = location;
  const document = {currentScript: null, readyState: 'loading', visibilityState: 'visible',
    addEventListener: (event, fn) => documentEvents.set(event, fn)};
  const json = body => ({ok:true, json:async () => body, text:async () => JSON.stringify(body)});
  const fetch = async (url, options) => {
    calls.push({url, options});
    if (url.endsWith('/api/inject/due')) return json({items:[
      {id:'different-topic', analysis_id:'analysis-a', session:'grp_old'},
      {id:'different-record', analysis_id:'analysis-b', session:'grp_open'},
      {id:'correct', analysis_id:'analysis-a', session:'grp_open', req_id:'req-a'}
    ]});
    return json({ok:false});
  };
  vm.runInNewContext(source, {window, document, location, URL, fetch,
    setTimeout() {}, setInterval: fn => timers.push(fn), clearTimeout() {}});
  window.AppShell.setRetryContext(() => ({analysisId:'analysis-a', session:'cc_group_grp_open', isCurrent: () => true}));
  documentEvents.get('DOMContentLoaded')();
  windowEvents.get('focus')();
  documentEvents.get('visibilitychange')();
  await settle();
  assert.ok(calls.length > 0);
  assert.ok(calls.every(call => call.url.endsWith('/api/inject')));
  timers[0]();
  await settle();
  const requests = calls.filter(call => call.url.endsWith('/api/instruction/prepare'));
  assert.equal(requests.length, 1);
  assert.equal(JSON.parse(requests[0].options.body).item_id, 'correct');
});
