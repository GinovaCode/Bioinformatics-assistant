'use strict';
const fs = require('node:fs');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const test = require('node:test');
const path = require('node:path');
const source = process.env.FRONTEND_SOURCE || path.join(__dirname, '../public/app.20261107fix1.js');
function deferred() { let resolve; const promise = new Promise(r => { resolve = r; }); return {promise, resolve}; }
function harness(appSource = source) {
  const elements = new Map(), calls = [], timers = new Map(); let timerId = 0;
  const element = id => { if (!elements.has(id)) elements.set(id, {innerHTML:'',textContent:'',style:{},classList:{add(){},remove(){},toggle(){}},addEventListener(){},querySelectorAll(){return []},querySelector(){return null},getAttribute(){return ''},appendChild(){},remove(){}}); return elements.get(id); };
  let backend = async url => url.includes('/sessions/switch') || url.includes('/sessions/new') ? {ok:true,session:{id:'B'}} : url.includes('/sessions/state') ? {ok:true,current:'B',session:{id:'B',mode:'server',run_dir:'/B',identify:{mode:'server',result:{}}},sessions:[]} : url.includes('/place') ? {ok:true,mode:'server',server:{}} : {};
  const c = vm.createContext({console, URL, Date, JSON, Promise, AbortController, location:{href:'http://local/',pathname:'/'},navigator:{},window:{addEventListener(){}},document:{currentScript:null,addEventListener(){},querySelector:element,querySelectorAll(){return []},getElementById:id=>element('#'+id),createElement(){return element('new')}},prompt:()=>'',setTimeout(fn,ms){ const id=++timerId;timers.set(id,{fn,ms});return id;},setInterval(fn,ms){const id=++timerId;timers.set(id,{fn,ms});return id;},clearTimeout:id=>timers.delete(id),clearInterval:id=>timers.delete(id),fetch:async (url,opts)=>{calls.push({url,opts});let body=await backend(url,opts);if (url.includes('/sessions/switch') && !Object.keys(body).length) body={ok:true,session:{id:JSON.parse(opts.body).id}};if (url.includes('/sessions/new') && !Object.keys(body).length) body={ok:true,session:{id:'B'}};return {ok:true,text:async()=>JSON.stringify(body)};}});
  vm.runInContext(fs.readFileSync(appSource,'utf8').replace(/boot\(\);\s*$/, ''), c);
  const run = s => vm.runInContext(s,c);
  run("S.sessionId='A'; S.runDir='/A'; S.needsRemote=true; toast=()=>{}; renderAll=()=>{}; renderPlace=()=>{}; renderSessionsBar=()=>{}; setStageVisible=()=>{}; renderFlow=()=>{}; renderRunCtl=()=>{};");
  return {c,run,calls,timers,e:element,setBackend:fn=>{backend=fn}};
}
test('switch and new clear all record execution evidence before render', async()=>{
  for (const action of ["switchSession('B')",'newSession()']) {
    const h=harness(); h.run("S.delegated={step:'pipeline1',title:'OLD',ok:true}; S.remoteSteps={pipeline1:{status:'done'}}; S.running=true; S.blocked=true; S.flow={stages:[{id:'pipeline1',status:'done'}]}; S.batch={done:true}; S.execTasks=[{id:'old'}]; S.artifacts=[{rel:'old'}]; S.variants={old:1}; S.lastQuery={old:1}; S.dirtyParams={old:1};");
    await h.run(action);
    assert.equal(h.run('S.delegated'),null); assert.equal(h.run('S.running'),false); assert.equal(h.run('S.batch'),null); assert.equal(h.run('S.flow'),null);
    for (const field of ['remoteSteps','execTasks','artifacts','variants','lastQuery','dirtyParams']) assert.equal(h.run('Object.keys(S.'+field+').length'),0, field);
  }
});
test('late flow and status responses never overwrite B', async()=>{
  const h=harness(), d=deferred();h.setBackend(async url=>url.includes('/flow')?d.promise:{});
  const pending=h.run('loadFlow()'); await h.run("switchSession('B')"); h.run("S.flow={marker:'B'}"); d.resolve({marker:'A'});await pending;
  assert.equal(h.run('S.flow.marker'),'B');
});
test('run request explicitly binds session and late result cannot delegate after switch', async()=>{
  const h=harness(),d=deferred();h.run('renderRun=()=>{}');h.setBackend(async url=>url.includes('/api/run')?d.promise:{});
  const p=h.run("runStep('pipeline1')"); const req=h.calls.find(x=>x.url.includes('/api/run'));
  await h.run("switchSession('B')"); d.resolve({ok:true,delegated:true,instruction:'A'});await p;
  assert.equal(h.run('S.delegated'),null); assert.equal(JSON.parse(req.opts.body).session_id,'A'); assert.match(req.url,/session=A/);
});
test('batch sleep cannot submit later steps against newly selected record',async()=>{
  const h=harness();h.run("renderRun=()=>{}; S.picked=['pipeline1','pipeline2']; S.flow={order:['pipeline1','pipeline2']};");
  h.setBackend(async()=>({ok:true}));const p=h.run('runPickedSteps()');await new Promise(r=>setImmediate(r));
  const sleep=[...h.timers.values()].find(t=>t.ms===1200);assert.ok(sleep);await h.run("switchSession('B')");sleep.fn();await new Promise(r=>setImmediate(r));
  assert.equal(h.calls.filter(x=>x.url.includes('/api/run')).length,1);await p;assert.equal(h.run('S.batch'),null);
});
test('already queued collapsed and poll callbacks cannot operate on B',async()=>{
  const h=harness();h.run('renderRun=()=>{}; persistCollapsed(); startPolling()');await new Promise(r=>setImmediate(r));const queued=[...h.timers.values()];await h.run("switchSession('B')");const n=h.calls.length;
  for(const t of queued) await t.fn();await new Promise(r=>setImmediate(r));assert.equal(h.calls.length,n);
});
test('server writeback is authoritative and card details do not expand inline',()=>{
  const h=harness();
  h.run("S.needsRemote=true; S.steps=[{id:'doublet',title:'去双'}]; S.flow={stages:[{id:'doublet',title:'去双',status:'failed',subs:[{title:'DoubletFinder',status:'failed',note:'failed reason'}],artifacts:[],deps:[]}]}; S.remoteSteps={doublet:{id:'doublet',status:'done',log:'done'}}; S.running=true; S.step='doublet'; S.code=1; renderRun()");
  assert.match(h.e('#stepsHost').innerHTML,/stcard done/);
  assert.match(h.e('#stepsHost').innerHTML,/data-stepdetail="doublet"/);
  assert.doesNotMatch(h.e('#stepsHost').innerHTML,/<details/);
  assert.doesNotMatch(h.e('#stepsHost').innerHTML,/failed reason/);
});
test('renderRun refuses delegated evidence owned by another record',()=>{
  const h=harness();h.run("S.delegated={sessionId:'OTHER',title:'OLD',ok:true}; S.steps=[]; renderRun()");assert.doesNotMatch(h.e('#runState').textContent,/OLD/);
});
test('late status, identify wait, and shell acknowledgement cannot mutate B',async()=>{
  for (const operation of ['pollStatus()', 'startWaitWriteback()', "runStep('pipeline1')"]) {
    const h=harness(), d=deferred();h.run('renderRun=()=>{}');
    if (operation.includes('runStep')) { h.c.window.AppShell={inject:()=>d.promise};h.setBackend(async url=>url.includes('/api/run')?{delegated:true,instruction:'A'}:{}); }
    else h.setBackend(async url=>url.includes('/status')||url.includes('/envcheck')?d.promise:{});
    const p=h.run(operation);await new Promise(r=>setImmediate(r));await h.run("switchSession('B')");
    d.resolve({ok:true,status:'running',step:'old',steps:[{id:'old',status:'done'}],result:{imported_at:new Date().toISOString()}});
    await p;await new Promise(r=>setImmediate(r));
    assert.equal(h.run('S.running'),false,operation);assert.equal(h.run('S.delegated'),null,operation);assert.equal(h.run('Object.keys(S.remoteSteps).length'),0,operation);assert.notEqual(h.run('S.identifyPhase'),'timeout');
  }
});
test('normal delegated run still injects once and keeps owned evidence',async()=>{
 const h=harness();let sends=0;h.run('renderRun=()=>{}');h.c.window.AppShell={inject:async()=>{sends++;return {ok:true}}};h.setBackend(async url=>url.includes('/api/run')?{delegated:true,instruction:'A'}:{});
 await h.run("runStep('pipeline1')");assert.equal(sends,1);assert.equal(h.run('S.delegated.sessionId'),'A');assert.equal(h.run('S.delegated.ok'),true);
});
test('overlapping record transitions are serialized',async()=>{
 const h=harness(),d=deferred();h.setBackend(async url=>url.includes('/sessions/switch')?d.promise:{});
 const p=h.run("switchSession('B')");await h.run("switchSession('C')");assert.equal(h.calls.filter(x=>x.url.includes('/sessions/switch')).length,1);d.resolve({ok:true,session:{id:'B'}});await p;assert.equal(h.run('S.sessionId'),'B');
});
test('A to B to A still rejects first A lifetime response',async()=>{
 const h=harness(),d=deferred();h.setBackend(async url=>url.includes('/flow')?d.promise:{});const p=h.run('loadFlow()');await h.run("switchSession('B')");await h.run("switchSession('A')");h.run("S.flow={marker:'new-A'}");d.resolve({marker:'old-A'});await p;assert.equal(h.run('S.flow.marker'),'new-A');
});
test('late run-directory response does not rehydrate the wrong record',async()=>{
 const h=harness(),d=deferred();h.setBackend(async url=>url.includes('/run-dir')?d.promise:{});const p=h.run('loadRun()');await h.run("switchSession('B')");d.resolve({runDir:'/old-A',steps:[{id:'old'}],stepState:{status:'running'}});await p;assert.notEqual(h.run('S.runDir'),'/old-A');assert.equal(h.run('S.running'),false);
});
test('read session query and write session_id bind explicitly without replacing supplied targets',async()=>{
 const h=harness();h.setBackend(async()=>({ok:true}));await h.run("getJSON('/api/flow'); postJSON('/api/run',{step:'pipeline1'}); getJSON('/api/variants?session=OTHER')");await new Promise(r=>setImmediate(r));assert.match(h.calls[0].url,/session=A/);assert.equal(JSON.parse(h.calls[1].opts.body).session_id,'A');assert.match(h.calls[2].url,/session=OTHER/);
});


test('an explicit historical record keeps its stored result when ancillary reads fail', async () => {
  const h = harness(path.join(__dirname, '../public/app.js'));
  h.setBackend(async url => {
    if (url.includes('/api/sessions/state')) return {
      ok: true, current: 'A', viewing_session_id: 'B', sessions: [], identify_status: 'ready',
      session: {id: 'B', mode: 'server', identify: {req_id: 'identify-B', result: {marker: 'B'}}}
    };
    throw new Error('ancillary read failed');
  });
  await h.run("loadSessionsState('B', false)");
  await h.run('renderIdentify()');
  assert.equal(h.run('S.sessionId'), 'B');
  assert.equal(h.run('S.envCheck.marker'), 'B');
  assert.equal(h.calls.filter(x => x.url.includes('/api/sessions/state')).length, 1);
  assert.equal(h.calls.some(x => x.url.includes('/api/envcheck')), false);
});

test('a reused execution task does not send a second chat notification', async () => {
  const h = harness(path.join(__dirname, '../public/app.js'));
  h.run("renderRun=()=>{}; startPolling=()=>{}; injectToSession=()=>{throw new Error('duplicate notification')}");
  h.setBackend(async () => ({ok:true, delegated:true, duplicate_dispatch:true, exec_id:'ex-existing'}));
  await h.run("runStep('doublet')");
  assert.equal(h.run('S.running'), false);
  assert.equal(h.run('S.delegated'), null);
  assert.equal(h.calls.filter(x => x.url.includes('/api/run')).length, 1);
});

test('step status inquiry binds to the selected job and does not submit a run', async () => {
  const h = harness(path.join(__dirname, '../public/app.js'));
  h.run("injectToSession = (_text, _state, purpose, binding, title) => { window.sent = {purpose, binding, title}; return Promise.resolve({ok:true}); }");
  h.setBackend(async () => ({stages:[{id:'doublet', title:'逐样本去双与质控', exec_id:'ex-A', exec_req_id:'req-A'}]}));
  await h.run("pokeFlow('doublet', '再报一次')");
  assert.equal(h.run('window.sent.purpose'), 'observe');
  assert.equal(h.run('window.sent.binding.reqId'), 'req-A');
  assert.equal(h.run('window.sent.title'), '再报一次 · 逐样本去双与质控');
  assert.equal(h.calls.some(x => x.url.includes('/api/run')), false);
});
