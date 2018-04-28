//------------------------------------------------------------------------------// Epilog//------------------------------------------------------------------------------//------------------------------------------------------------------------------// Sentential Representation//------------------------------------------------------------------------------function symbolp (x) {return typeof x==='string'}function varp (x) {return typeof x==='string' && x.length!==0 && x[0]!==x[0].toLowerCase()}function constantp (x) {return typeof x==='string' && x.length!==0 && x[0]===x[0].toLowerCase()}function stringp (x) {return typeof x==='string' && x.length>1 && x[0]==='"' &&  x[x.length-1]==='"'}var counter = 0function newvar () {counter++;  return 'V' + counter}function newsym () {counter++;  return 'c' + counter}function seq () {var exp=new Array(arguments.length);  for (var i=0; i<arguments.length; i++) {exp[i]=arguments[i]};  return exp}function head (p) {return p[0]}function tail (l) {return l.slice(1,l.length)}function makeequality (x,y) {return seq('same',x,y)}function makeinequality (x,y) {return seq('distinct',x,y)}function makeprovable (p) {return seq('provable',p)}function makenegation (p) {return seq('not',p)}function makeconjunction (p,q) {if (p[0]==='and') {return p.concat(seq(q))};  return seq('and',p,q)}function makedisjunction (p,q) {if (p[0]==='or') {return p.concat(seq(q))};  return seq('or',p,q)}function makereduction (head,body) {return seq('reduction',head,body)}function makeimplication (head,body) {return seq('implication',head,body)}function makeimplication (head,body) {if (head.length===0) {return body};  if (head[0]==='and')     {return seq('implication').concat(tail(head)).concat(seq(body))};  return seq('implication',head,body)}function makeequivalence (head,body) {return seq('equivalence',head,body)}function makerule (head,body) {if (body.length===0) {return head};  if (body[0]==='and') {return seq('rule',head).concat(tail(body))};  return seq('rule',head,body)}function maketransition (head,body) {return seq('transition',head,body)}

function makeuniversal (variable,scope) {return seq('forall',variable,scope)}function makeexistential (variable,scope) {return seq('exists',variable,scope)}function makeconditional (p,x,y) {return seq('if',p,x,y)}function makeclause (p,q) {return seq('clause',p,q)}function makedefinition (head,body) {if (!symbolp(body) & body[0]=='and')     {return seq('definition',head).concat(tail(body))}  else {return seq('definition',head,body)}}function makestep (sentence,justification,p1,p2) {var exp = new Array(3);  exp[0] = 'step';  exp[1] = sentence;  exp[2] = justification;  if (p1) {exp[3] = p1};  if (p2) {exp[4] = p2};  return exp}function makeproof () {var exp = new Array(1);  exp[0] = 'proof';  return exp}function maksatom (r,s) {return seq(r).concat(s)}function maksand (s) {if (s.length===0) {return 'true'};  if (s.length===1) {return s[0]};  return seq('and').concat(s)}function maksor (s) {if (s.length===0) {return 'false'};  if (s.length===1) {return s[0]};  return seq('or').concat(s)}function negate (p) {if (symbolp(p)) {return makenegation(p)};  if (p[0]==='not') {return p[1]};  return makenegation(p)}function adjoin (x,s) {if (!findq(x,s)) {s.push(x)};  return s}

function newadjoin (x,s)
 {if (!findq(x,s)) {return s.concat(seq(x))};  return s}function concatenate (l1,l2) {return l1.concat(l2)}function findq (x,s) {for (var i=0; i<s.length; i++) {if (x===s[i]) {return true}};  return false}function find (x,s) {for (var i=0; i<s.length; i++) {if (equalp(x,s[i])) {return true}};  return false}function subset (s1,s2) {for (var i=0; i<s1.length; i++)      {if (!find(s1[i],s2)) {return false}};  return true}function difference (l1, l2) {var answer = seq();  for (var i=0; i<l1.length; i++)      {if (!find(l1[i],l2)) {answer[answer.length] = l1[i]}};  return answer}function subst (x,y,z) {if (z===y) {return x};  if (symbolp(z)) {return z};  var exp = new Array(z.length);  for (var i=0; i<z.length; i++)      {exp[i] = subst(x,y,z[i])};  return exp}function substitute (p,q,r) {if (symbolp(r)) {if (r===p) {return q} else {return r}};  var exp = seq();  for (var i=0; i<r.length; i++)      {exp[exp.length] = substitute(p,q,r[i])};  if (equalp(exp,p)) {return q} else {return exp}}function substitutions (p,q,r) {if (symbolp(r)) {if (r===p) {return seq(r,q)} else {return seq(r)}};  return substitutionsexp(p,q,r,0)}function substitutionsexp (p,q,r,n) {if (n===r.length) {return seq(seq())};  var firsts = substitutions(p,q,r[n]);  var rests = substitutionsexp(p,q,r,n+1);  var results = seq();  for (var i=0; i<firsts.length; i++)      {for (var j=0; j<rests.length; j++)           {exp = seq(firsts[i]).concat(rests[j]);            results[results.length] = exp;            if (equalp(exp,p)) {results[results.length] = q}}}  return results}function vars (x) {return varsexp(x,seq())}function varsexp (x,vs) {if (varp(x)) {return adjoin(x,vs)};  if (symbolp(x)) {return vs};  for (var i=0; i<x.length; i++) {vs = varsexp(x[i],vs)};  return vs}

function freevarsexp (x,al,vs) {if (varp(x)) {if (al[x]==null || al[x].length===0) {return adjoin(x,vs)}};  if (symbolp(x)) {return vs};  for (var i=0; i<x.length; i++) {vs = freevarsexp(x[i],al,vs)};  return vs}function constants (x) {return constantsexp(x,seq())}function constantsexp (x,vs) {if (varp(x)) {return vs};  if (symbolp(x)) {return adjoin(x,vs)};  for (var i=1; i<x.length; i++) {vs = constantsexp(x[i],vs)};  return vs}function equalp (p,q) {if (p===true || p===false) {return p===q};
  if (q===true || q===false) {return false};
  if (symbolp(p)) {if (symbolp(q)) {return p===q} else {return false}};  if (symbolp(q)) {return false};  if (p.length!==q.length) {return false};  for (var i=0; i<p.length; i++) {if (!equalp(p[i],q[i])) {return false}};  return true}

function pseudogroundp (exp,al) {if (varp(exp))
     {var dum = al[exp];      if (dum && dum.length > 0) {return pseudogroundp(dum[0],dum[1])};      return false};  if (symbolp(exp)) {return true};  for (var i=0; i<exp.length; i++)      {if (!pseudogroundp(exp[i],al)) {return false}};  return true}//------------------------------------------------------------------------------// Linked Lists//------------------------------------------------------------------------------var nil = 'nil'function nullp (l) {return l==='nil'}

function cons (x,l) {var cell = new Array(2);  cell[0] = x;  cell[1] = l;  return cell}

function car (l) {return l[0]}function cdr (l) {return l[1]}function list () {var exp=nil;  for (var i=arguments.length; i>0; i--)      {exp=cons(arguments[i-1],exp)};  return exp}function len (l) {var n = 0;  for (var m=l; m!=nil; m = cdr(m)) {n = n+1};  return n}function memberp (x,l) {if (nullp(l)) {return false};  if (equalp(car(l),x)) {return true};  if (memberp(x,cdr(l))) {return true};  return false}

function amongp (x,y)
 {if (symbolp(y)) {return x==y};
  for (var i=0; i<y.length; i++) {if (amongp(x,y[i])) {return true}}
  return false}function nreverse (l) {if (nullp(l)) {return nil}  else {return nreversexp(l,nil)}}function nreversexp (l,ptr) {if (cdr(l)===nil) {l[1] = ptr; return l};  var rev = nreversexp(cdr(l),l);  l[1] = ptr;  return rev}function acons (x,y,al) {return cons(cons(x,y),al)}function assoc (x,al) {if (nullp(al)) {return false};  if (x===car(car(al))) {return car(al)};  return assoc(x,cdr(al))}//------------------------------------------------------------------------------// matcher
// unifier
// plug
// standardize
//------------------------------------------------------------------------------function matcher (x,y) {return match(x,y,nil)}function match (x,y,bl) {if (x===y) {return bl};  if (varp(x)) {return matchvar(x,y,bl)};  if (symbolp(x)) {return false};  return matchexp(x,y,bl)}function matchvar (x,y,bl) {var dum = assoc(x,bl);  if (dum!==false) {return match(cdr(dum),y,bl)};  if (x===matchval(y,bl)) {return bl};  return acons(x,y,bl)}function matchval (y,bl) {if (varp(y))     {var dum = assoc(y,bl);      if (dum!==false) {return matchval(cdr(dum),bl)};      return y};  return y}function matchexp(x,y,bl) {if (symbolp(y)) {return false};  var m = x.length;  var n = y.length;    if (m!==n) {return false};  for (var i=0; i<m; i++)      {bl = match(x[i],y[i],bl);       if (bl===false) {return false}};  return bl}//------------------------------------------------------------------------------var unifications = 0;
function unifier (x,y) {return unify(x,y,nil)}function unify (x,y,bl) {if (x===y) {return bl};  if (varp(x)) {return unifyvar(x,y,bl)};  if (symbolp(x)) {return unifyatom(x,y,bl)};  return unifyexp(x,y,bl)}function unifyvar (x,y,bl) {var dum = assoc(x,bl);  if (dum!==false) {return unify(cdr(dum),y,bl)};  if (occurs(x,y,bl)) {return false};  return acons(x,y,bl)}function occurs (x,y,al)
 {if (varp(y))
     {if (x===y) {return true};      var dum = assoc(y,al);
      if (dum!==false) {return occurs(x,cdr(dum),al)};
      return false};
  if (symbolp(y)) {return false};
  for (var i=0; i<y.length; i++)      {if (occurs(x,y[i],al)) {return true}};
  return false}function unifyatom (x,y,bl) {if (varp(y)) {return unifyvar(y,x,bl)};
  return false}function unifyexp(x,y,bl) {if (varp(y)) {return unifyvar(y,x,bl)}  if (symbolp(y)) {return false};  if (x.length!==y.length) {return false};  for (var i=0; i<x.length; i++)      {bl = unify(x[i],y[i],bl);       if (bl===false) {return false}};  return bl}

//------------------------------------------------------------------------------function plug (x,bl) {if (varp(x)) {return plugvar(x,bl)};  if (symbolp(x)) {return x};  return plugexp(x,bl)}function plugvar (x,bl) {var dum = assoc(x,bl);  if (dum===false) {return x};  return plug(cdr(dum),bl)}function plugexp (x,bl) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = plug(x[i],bl)};  return exp}

//------------------------------------------------------------------------------var alist;function standardize (x) {alist = nil;  return standardizeit(x)}function standardizeit (x) {if (varp(x)) {return standardizevar(x)};  if (symbolp(x)) {return x};  return standardizeexp(x)}function standardizevar (x) {var dum = assoc(x,alist);  if (dum!==false) {return cdr(dum)};  var rep = newvar();  alist = acons(x,rep,alist);  return rep}function standardizeexp (x) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = standardizeit(x[i])};  return exp}//------------------------------------------------------------------------------// maatcher
// vnifier// pluug//------------------------------------------------------------------------------function maatcher (x,y) {return maatchify(x,seq(),y,seq(),seq())}function maatchifyp (x,al,y,bl,ol)
 {unifications++;
  return maatchify(x,al,y,bl,ol)}function maatchify (x,al,y,bl,ol) {if (varp(x)) {return maatchvar(x,al,y,bl,ol)};  if (symbolp(x)) {return (x===y)};  return maatchexp(x,al,y,bl,ol)}function maatchvar (x,al,y,bl,ol) {if (x===y && al===bl) {return true};  var dum = al[x];  if (dum && dum.length > 0) {return maatchify(dum[0],dum[1],y,bl,ol)};  if (x===maatchval(y,bl)) {return true};  ol[ol.length] = setbdg(x,al,y,bl);  return true}function maatchval (y,bl) {if (varp(y))     {var dum = bl[y];      if (dum!==null && dum[1]!==nil) {return maatchval(dum[0],dum[1])};      return y};  return y}function maatchexp(x,al,y,bl,ol) {if (symbolp(y)) {return false};  var m = x.length;  var n = y.length;    if (m!==n) {return false};  for (var i=0; i<m; i++)      {if (!maatchify(x[i],al,y[i],bl,ol)) {return false}};  return true}//------------------------------------------------------------------------------function vnifier (x,y) {var ol = seq();  if (vnify(x,seq(),y,seq(),ol)) {return ol};  backup(ol);  return false}function vnifyp (x,al,y,bl,ol) {unifications = unifications + 1;
  if (vnify(x,al,y,bl,ol)) {return true};  backup(ol);  return false}function vnify (x,al,y,bl,ol) {if (varp(x)) {return vnifyvar(x,al,y,bl,ol)};  if (symbolp(x)) {return vnifysymbol(x,al,y,bl,ol)};  return vnifyexp(x,al,y,bl,ol)}function vnifyvar (x,al,y,bl,ol) {if (x===y && al===bl) {return true};  var dum = al[x];  if (dum && dum.length > 0) {return vnify(dum[0],dum[1],y,bl,ol)};  if (vccurs(x,al,y,bl)) {return false};  ol[ol.length] = setbdg(x,al,y,bl);  return true}function vccurs (x,al,y,bl)
 {if (varp(y))
     {if (x===y && al===bl) {return true};      var dum = bl[y];      if (dum && dum.length > 0) {return vccurs(x,al,dum[0],dum[1])};
      return false};
  if (symbolp(y)) {return false};
  for (var i=0; i<y.length; i++)      {if (vccurs(x,al,y[i],bl)) {return true}};
  return false}function vnifysymbol (x,al,y,bl,ol) {if (x===y) {return true};  if (varp(y)) {return vnifyvar(y,bl,x,al,ol)};  return false}function vnifyexp(x,al,y,bl,ol) {if (varp(y)) {return vnifyvar(y,bl,x,al,ol)}  if (symbolp(y)) {return false};  if (x.length!==y.length) {return false};  for (var i=0; i<x.length; i++)      {if (!vnify(x[i],al,y[i],bl,ol)) {return false}};  return true}//------------------------------------------------------------------------------function getbdg (x,al) {return al[x]}function setbdg (x,al,y,bl) {var bdg = seq(y,bl);  al[x] = bdg;  return bdg}function backup (bl) {for (var i=0; i<bl.length; i++) {bl[i].length = 0}}function pluug (x,al,bl) {if (varp(x)) {return pluugvar(x,al,bl)};  if (symbolp(x)) {return x};  return pluugexp(x,al,bl)}function pluugvar (x,al,bl) {var dum = al[x];  if (dum && dum.length > 0) {return pluug(dum[0],dum[1],bl)};  if (al===bl) {return x};  var rep = newvar();  al[x] = seq(rep,bl);  return rep};function pluugexp (x,al,bl) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = pluug(x[i],al,bl)};  return exp}//------------------------------------------------------------------------------// Storage//------------------------------------------------------------------------------var indexing = truevar dataindexing = truevar ruleindexing = truefunction definetheory (source,data) {emptytheory(source);  definemore(source,data);  return true}function definefacts (source,data) {emptytheory(source);  definemorefacts(source,data);  return true}function definerules (source,data) {emptytheory(source);  definemorerules(source,data);  return true}function definemore (theory,data) {for (var i=0; i<data.length; i++) {insert(data[i],theory)};  return true}function definemorefacts (theory,data)
 {for (var i=0; i<data.length; i++) {insertfact(data[i],theory)};
  return theory}

function definemorerules (theory,data)
 {for (var i=0; i<data.length; i++) {insertrule(data[i],theory)};
  return theory}

function emptytheory (theory) {theory.splice(0,theory.length);  for (var x in theory) {delete theory[x]};  return true}//------------------------------------------------------------------------------function save (datum,theory) {data = indexps(datum,theory);  if (find(datum,data)) {return false};  return insert(datum,theory)}function savefact (datum,theory) {data = indexps(datum,theory);  if (find(datum,data)) {return false};  return insertfact(datum,theory)}function saverule (datum,theory) {data = indexps(datum,theory);  if (find(datum,data)) {return false};  return insertrule(datum,theory)}function drop (p,theory) {data = indexps(p,theory);  for (var i=0; i<data.length; i++)      {var datum = data[i];
       if (equalp(datum,p)) {uninsert(datum,theory); return datum}};  return false}function dropfact (p,theory) {data = indexps(p,theory);  for (var i=0; i<data.length; i++)      {var datum = data[i];
       if (equalp(datum,p)) {uninsertfact(datum,theory); return datum}};  return false}function droprule (p,theory) {data = viewindexps(p,theory);  for (var i=0; i<data.length; i++)      {var datum = data[i];
       if (equalp(datum,p)) {uninsertrule(datum,theory); return datum}};  return false}function eliminate (object,theory) {var data = indexees(object,theory).concat();  for (var i=0; i<data.length; i++)      {if (data[i][1]===object) {uninsert(data[i],theory)}};  return object}function eliminatefacts (object,theory) {var data = indexees(object,theory).concat();  for (var i=0; i<data.length; i++)      {if (data[i][1]===object) {uninsertfact(data[i],theory)}};  return object}function eliminaterules (object,theory) {var data = indexees(object,theory).concat();  for (var i=0; i<data.length; i++)      {if (data[i][1]===object) {uninsertrule(data[i],theory)}};  return object}//------------------------------------------------------------------------------function insert(p,theory) {addcontent(p,theory);  if (indexing) {index(p,p,theory)};  return p}function insertfact (p,theory) {addcontent(p,theory);  if (dataindexing) {fullindex(p,p,theory)};  return p}

function insertrule (p,theory) {addcontent(p,theory);  if (ruleindexing) {relindex(p,p,theory)};  return p}

function uninsert (p,theory) {if (indexing) {unindex(p,p,theory)};  return remcontent(p,theory)}

function uninsertfact (p,theory) {if (dataindexing) {unindex(p,p,theory)};  return remcontent(p,theory)}

function uninsertrule (p,theory) {if (ruleindexing) {unindex(p,p,theory)};  return remcontent(p,theory)}

//------------------------------------------------------------------------------function addcontent (p,theory) {theory.push(p);  return p}function remcontent (p,theory) {for (var i=0; i<theory.length; i++)      {if (theory[i]===p) {theory.splice(i,1); return p}};  return false}//------------------------------------------------------------------------------function indexps (p,theory) {if (indexing) {return fullindexps(p,theory)} else {return theory}}function envindexps (p,al,theory) {if (dataindexing) {return dataindexps(p,al,theory)} else {return theory}}function envvndexps (p,al,theory) {if (dataindexing) {return datavndexps(p,al,theory)} else {return theory}}

function viewindexps (p,theory) {if (ruleindexing) {return ruleindexps(p,theory)} else {return theory}}

//------------------------------------------------------------------------------function index (x,p,theory) {if (varp(x)) {return p};  if (symbolp(x)) {return indexsymbol(x,p,theory)};  for (var i=0; i<x.length; i++) {index(x[i],p,theory)};  return p}function fullindex (x,p,theory) {if (varp(x)) {return p};  if (symbolp(x)) {return indexsymbol(x,p,theory)};  for (var i=0; i<x.length; i++) {fullindex(x[i],p,theory)};  return p}function relindex (x,p,theory) {if (varp(x)) {return p};  if (symbolp(x)) {return indexsymbol(x,p,theory)};  if (x[0]==='rule') {return relindex(x[1],p,theory)};  return relindex(x[0],p,theory)}function indexsymbol (x,p,theory) {if (x==='map') {return p};  if (!isNaN(Number(x))) {return p};  var data = theory[x];  if (data) {data.push(p)} else {theory[x] = seq(p)};  return p}function unindex (x,p,theory) {if (varp(x)) {return p};  if (symbolp(x)) {return unindexsymbol(x,p,theory)};  for (var i=0; i<x.length; i++) {unindex(x[i],p,theory)};  return p}function unindexsymbol (x,p,theory) {if (theory[x]) {return remcontent(p,theory[x])}}function fullindexps (p,theory) {if (varp(p)) {return theory};  if (symbolp(p)) {return indexees(p,theory)};  for (var i=1; i<p.length; i++)      {if (symbolp(p[i]) && !varp(p[i])) {return indexees(p[i],theory)}};  return indexees(p[0],theory)}function dataindexps (p,al,facts) {if (varp(p)) {return facts};  if (symbolp(p)) {return indexees(p,facts)};  var best = indexees(p[0],facts);  for (var i=1; i<p.length; i++)      {var dum = unival(p[i],al);
       dum = dataindexps(dum,al,facts);       if (dum.length<best.length) {best = dum}};  return best}
function unival (x,al) {if (!varp(x)) {return x};  var dum = assoc(x,al);  if (dum) {return unival(cdr(dum),al)};  return x}function datavndexps (p,al,facts) {if (varp(p))
     {var dum = al[p];      if (dum && dum.length>0) {return datavndexps(car(dum),cdr(dum),facts)};
      return facts};  if (symbolp(p)) {return indexees(p,facts)};
  var best = indexees(p[0],facts);  for (var i=1; i<p.length; i++)      {var dum = datavndexps(p[1],al,facts);       if (dum.length<best.length) {best = dum}};  return best}

function ruleindexps (p,theory) {if (varp(p)) {return theory};  if (symbolp(p)) {return indexees(p,theory)};  if (p[0]==='rule') {return ruleindexps(p[1],theory)};
  return indexees(p[0],theory)}function indexees (x,theory) {if (x==='map') {return theory};
  if (!isNaN(Number(x))) {return theory};
  var data = theory[x];  if (data) {return data} else {return seq()}}
//------------------------------------------------------------------------------function uniquify (ins) {var outs = seq();  for (var i=0; i<ins.length; i++) {outs = adjoinit(ins[i],outs)};  return outs}function vniquify (ol) {var sl = ol.sort();  var nl =seq();  var last = false;  for (var i=0; i<sl.length; i++)      {if (!equalp(sl[i],last)) {last = sl[i]; nl[nl.length] = last}};  return nl}function adjoinit (x,s) {if (find(x,s)) {return s} else {s[s.length] = x; return s}}function nconc (l1,l2) {for (var i=0; i<l2.length; i++) {l1.push(l2[i])};  return l1}

//------------------------------------------------------------------------------

function viewp (r,rules)
 {if (ruleindexing) {return (indexees(r,rules).length!==0)};
  for (var i=0; i<rules.length; i++)
      {if (operator(rules[i])===r) {return true}};
  return false}

function getbases (data)
 {var tables = seq();
  for (var i=0; i<data.length; i++)
      {tables = adjoin(operator(data[i]),tables)};
  return tables}

function getviews (data)
 {var tables = seq();
  for (var i=0; i<data.length; i++)
      {tables = adjoin(operator(data[i]),tables)};
  return tables}

function makepattern (relation,arity)
 {var pattern = seq(relation);
  for (var j=1; j<=arity; j++)
      {pattern[j] = 'X' + j};
  return pattern}

function getfactarity (relation,facts) {for (var i=0; i<facts.length; i++)      {if (facts[i][0]===relation) {return facts[i].length-1}};  return 0}

function getrulearity (relation,rules) {for (var i=0; i<rules.length; i++)      {if (rules[i]===relation) {return 0};
       if (symbolp(rules[i])) {continue};
       if (rules[i][0]===relation) {return rules[i].length-1};
       if (rules[i][0]==='rule' && !symbolp(rules[i][1]) && rules[i][1][0]===relation)
          {return rules[i][1].length-1}};  return 0}

function sentences (relation,data) {var results = seq();  for (var i=0; i<data.length; i++)      {if (operator(data[i])===relation) {results.push(data[i])}};  return results}

function sentencen (m,n,relation,data) {var results = sentences(relation,data);
  if (results.length>=n) {return results.slice(m,n)};
  if (results.length>=m) {return results.slice(m)};
  return seq()}

function viewfacts (relation,facts,rules) {var pattern = makepattern(relation,getrulearity(relation,rules));
  return sortfinds(pattern,pattern,facts,rules)}

//------------------------------------------------------------------------------// basefindp// basefindx// basefinds//------------------------------------------------------------------------------var inferences = 0;function basefindp (query,facts,rules) {return (basefindx('true',query,facts,rules)==='true')}function basefindx (result,query,facts,rules) {return baseone(result,query,seq(),seq(),nil,facts,rules)}function basefinds (result,query,facts,rules) {var answers = seq();  baseall(result,query,seq(),seq(),nil,answers,facts,rules);  return uniquify(answers)}

function basefindn (m,n,result,query,facts,rules)
 {var results = basefinds(result,query,facts,rules);
  if (results.length>=n) {return results.slice(m,n)};
  if (results.length>=m) {return results.slice(m)};
  return seq()}

//------------------------------------------------------------------------------

function baseone (x,p,pl,al,cont,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return baseoneatom(x,p,pl,al,cont,facts,rules)}  if (p[0]==='same') {return baseonesame(x,p,pl,al,cont,facts,rules)}  if (p[0]==='distinct') {return baseonedistinct(x,p,pl,al,cont,facts,rules)}  if (p[0]==='not') {return baseonenot(x,p,pl,al,cont,facts,rules)}  if (p[0]==='and') {return baseoneand(x,p,pl,al,cont,facts,rules)}  if (p[0]==='or') {return baseoneor(x,p,pl,al,cont,facts,rules)}
  if (pseudogroundp(p,al)) {return baseoneground(x,p,pl,al,cont,facts,rules)};  return baseonedb(x,p,pl,al,cont,facts,rules)}
function baseoneatom (x,p,pl,al,cont,facts,rules) {if (p==='true') {return baseoneexit(x,pl,al,cont,facts,rules)};  if (p==='false') {return false};
  return baseonedb(x,p,pl,al,cont,facts,rules)}function baseonesame (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {if (answer = baseoneexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};  return false}function baseonedistinct (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return baseoneexit(x,pl,al,cont,facts,rules)}

function baseonenot (x,p,pl,al,cont,facts,rules) {if (baseone(x,p[1],seq(),al,nil,facts,rules)==false)
     {return baseoneexit(x,pl,al,cont,facts,rules)};
  return false}function baseoneand (x,p,pl,al,cont,facts,rules) {return baseoneexit(x,concatenate(tail(p),pl),al,cont,facts,rules)}function baseoneor (x,p,pl,al,cont,facts,rules) {var answer;
  for (var i=0; i<p.length; i++)      {if (answer = baseone(x,p[i],pl,al,cont,facts,rules)) {return answer}};  return false}function baseoneground (x,p,pl,al,cont,facts,rules) {if (baseonedb(x,p,seq(),al,nil,facts,rules))
     {return baseoneexit(x,pl,al,cont,facts,rules)};
  return false}function baseonedb (x,p,pl,al,cont,facts,rules)
 {var answer;
  if (answer = baseonebackground(x,p,pl,al,cont,facts,rules)) {return answer};  if (answer = baseoners(x,p,pl,al,cont,facts,rules)) {return answer};
  return false}function baseonebackground (x,p,pl,al,cont,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (vnifyp(data[i],bl,p,al,ol))          {if (answer = baseoneexit(x,pl,al,cont,facts,rules))
              {backup(ol); return answer};           backup(ol)}};  return false}function baseoners (x,p,pl,al,cont,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pl,al,cont),cont);               if (answer = baseoneexit(x,ql,bl,nc,facts,rules))                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = baseoneexit(x,pl,al,cont,facts,rules))                    {backup(ol); return answer};                 backup(ol)}}}  return false}function baseoneexit (x,pl,al,cont,facts,rules) {if (pl.length!==0) {return baseone(x,pl[0],tail(pl),al,cont,facts,rules)};  if (nullp(cont)) {return pluug(x,al,al)};  return baseoneexit(x,car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}//------------------------------------------------------------------------------function baseall (x,p,pl,al,cont,results,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return baseallatom(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='same') {return baseallsame(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='distinct') {return basealldistinct(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='not') {return baseallnot(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='and') {return basealland(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='or') {return baseallor(x,p,pl,al,cont,results,facts,rules)}  if (pseudogroundp(p,al)) {return baseallground(x,p,pl,al,cont,results,facts,rules)}  return basealldb(x,p,pl,al,cont,results,facts,rules)}
function baseallatom (x,p,pl,al,cont,results,facts,rules) {if (p==='true') {return baseallexit(x,pl,al,cont,results,facts,rules)};  if (p==='false') {return false};  return basealldb(x,p,pl,al,cont,results,facts,rules)}function baseallsame (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {baseallexit(x,pl,al,cont,results,facts,rules); backup(ol)}}function basealldistinct (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return baseallexit(x,pl,al,cont,results,facts,rules)}function baseallnot (x,p,pl,al,cont,results,facts,rules) {if (baseone(x,p[1],seq(),al,nil,facts,rules)==false)
     {baseallexit(x,pl,al,cont,results,facts,rules)}}function basealland (x,p,pl,al,cont,results,facts,rules) {baseallexit(x,concatenate(tail(p),pl),al,cont,results,facts,rules)}function baseallor (x,p,pl,al,cont,results,facts,rules) {for (var i=1; i<p.length; i++)
      {baseall(x,p[i],pl,al,cont,results,facts,rules)}}function baseallground (x,p,pl,al,cont,results,facts,rules) {if (baseonedb(x,p,seq(),al,nil,facts,rules))
     {return baseallexit(x,pl,al,cont,results,facts,rules)};
  return false}

function basealldb (x,p,pl,al,cont,results,facts,rules)
 {baseallbackground(x,p,pl,al,cont,results,facts,rules);  baseallrs(x,p,pl,al,cont,results,facts,rules);
  return false}
function baseallbackground (x,p,pl,al,cont,results,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {baseallexit(x,pl,al,cont,results,facts,rules);           backup(ol)}}}function baseallrs (x,p,pl,al,cont,results,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pl,al,cont),cont);               baseallexit(x,ql,bl,nc,results,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {baseallexit(x,pl,al,cont,results,facts,rules);                 backup(ol)}}}}function baseallexit (x,pl,al,cont,results,facts,rules) {if (pl.length!==0) {return baseall(x,pl[0],tail(pl),al,cont,results,facts,rules)};  if (nullp(cont)) {results.push(pluug(x,al,al)); return true};  return baseallexit(x,car(cont)[0],car(cont)[1],car(cont)[2],results,facts,rules)}

//------------------------------------------------------------------------------// testfindp// testfindx// testfinds//------------------------------------------------------------------------------

function testfindp (query,facts,rules) {return (testfindx('true',query,facts,rules)==='true')}function testfindx (result,query,facts,rules) {return testone(result,query,seq(),seq(),nil,facts,rules)}
function testfinds (x,p,facts,rules)
 {var answers = testalls(x,seq(p),seq(),facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------

function testone (x,p,pl,al,cont,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return testoneatom(x,p,pl,al,cont,facts,rules)}  if (p[0]==='same') {return testonesame(x,p,pl,al,cont,facts,rules)}  if (p[0]==='distinct') {return testonedistinct(x,p,pl,al,cont,facts,rules)}  if (p[0]==='not') {return testonenot(x,p,pl,al,cont,facts,rules)}  if (p[0]==='and') {return testoneand(x,p,pl,al,cont,facts,rules)}  if (p[0]==='or') {return testoneor(x,p,pl,al,cont,facts,rules)}
  if (pseudogroundp(p,al)) {return testoneground(x,p,pl,al,cont,facts,rules)};  return testonedb(x,p,pl,al,cont,facts,rules)}
function testoneatom (x,p,pl,al,cont,facts,rules) {if (p==='true') {return testoneexit(x,pl,al,cont,facts,rules)};  if (p==='false') {return false};
  return testonedb(x,p,pl,al,cont,facts,rules)}function testonesame (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {if (answer = testoneexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};  return false}function testonedistinct (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return testoneexit(x,pl,al,cont,facts,rules)}

function testonenot (x,p,pl,al,cont,facts,rules) {if (testone(x,p[1],seq(),al,nil,facts,rules)==false)
     {return testoneexit(x,pl,al,cont,facts,rules)};
  return false}function testoneand (x,p,pl,al,cont,facts,rules) {return testoneexit(x,concatenate(tail(p),pl),al,cont,facts,rules)}function testoneor (x,p,pl,al,cont,facts,rules) {var answer;
  for (var i=0; i<p.length; i++)      {if (answer = testone(x,p[i],pl,al,cont,facts,rules)) {return answer}};  return false}function testoneground (x,p,pl,al,cont,facts,rules) {if (testonedb(x,p,seq(),al,nil,facts,rules))
     {return testoneexit(x,pl,al,cont,facts,rules)};
  return false}function testonedb (x,p,pl,al,cont,facts,rules)
 {var answer;
  if (answer = testonebackground(x,p,pl,al,cont,facts,rules)) {return answer};  if (answer = testoners(x,p,pl,al,cont,facts,rules)) {return answer};
  return false}function testonebackground (x,p,pl,al,cont,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (vnifyp(data[i],bl,p,al,ol))          {if (answer = testoneexit(x,pl,al,cont,facts,rules))
              {backup(ol); return answer};           backup(ol)}};  return false}function testoners (x,p,pl,al,cont,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pl,al,cont),cont);               if (answer = testoneexit(x,ql,bl,nc,facts,rules))                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = testoneexit(x,pl,al,cont,facts,rules))                    {backup(ol); return answer};                 backup(ol)}}}  return false}function testoneexit (x,pl,al,cont,facts,rules) {if (pl.length!==0) {return testone(x,pl[0],tail(pl),al,cont,facts,rules)};  if (nullp(cont)) {return pluug(x,al,al)};  return testoneexit(x,car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}//------------------------------------------------------------------------------
function testall (p,al,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return testallatom(p,al,facts,rules)}
  if (p[0]==='same') {return testallsame(p,al,facts,rules)}  if (p[0]==='distinct') {return testalldistinct(p,al,facts,rules)}  if (p[0]==='provable') {return testallprovable(p,al,facts,rules)}  if (p[0]==='not') {return testallnot(p,al,facts,rules)}  if (p[0]==='and') {return testalland(p,al,facts,rules)}  if (p[0]==='or') {return testallor(p,al,facts,rules)}  if (pseudogroundp(p,al)) {return testallground(p,al,facts,rules)}  if (!viewp(p[0],rules)) {return testallfacts(p,al,facts,rules)}
  if (compiledp(p[0])) {testallcompiled(p,al,facts,rules)}
  return testallrules(p,al,facts,rules)}
function testallatom (p,al,facts,rules) {if (p==='true') {return seq(p)};  if (p==='false') {return seq()};  return testallground(p,al,facts,rules)}

function testallsame (p,al,facts,rules) {var ol = seq();
  if (vnifyp(p[1],al,p[2],al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}
function testalldistinct (p,al,facts,rules) {var ol = seq();
  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return seq()};  return seq(pluug(p,al,al))}

function testallprovable (p,al,facts,rules) {p = testonedb(p,p[1],seq(),al,nil,facts,rules);
  if (p!==false) {return seq(p)};
  return seq()}
function testallnot (p,al,facts,rules) {p = pluug(p,al,al);
  if (testfindp(p[1],facts,rules)) {return seq()};
  return seq(p)}function testalland (p,al,facts,rules) {return testalls(p,tail(p),al,facts,rules)}

function testallor (p,al,facts,rules) {var results = seq();
  for (var i=1; i<p.length; i++)      {results = results.concat(testall(p[i],al,facts,rules))};
  return results}

function testallground (p,al,facts,rules) {p = pluug(p,al,al);
  if (testonedb('true',p,seq(),seq(),nil,facts,rules)==='true') {return seq(p)};
  return seq()}function testallfacts (p,al,facts,rules) {return envvndexps(p,al,facts)}

function testallcompiled (p,al,facts,rules) {var subroutine = eval('comp' + p[0]);
  return subroutine.apply(null,pluug(p,al,al).slice(1))}

function testallrules (p,al,facts,rules) {var results = seq();  var data = viewindexps(p,rules);
  for (var i=0; i<data.length; i++)      {if (data[i][0]==='rule')          {var bl = seq();
           var ol = seq();
           if (vnifyp(p,al,data[i][1],bl,ol))              {var head = data[i][1];
               var body = data[i].slice(2);
               var answers = testalls(head,body,bl,facts,rules);
               results = results.concat(answers);
               backup(ol)}}
       else {var ol = seq();
             if (maatchifyp(p,al,data[i],al,ol))
                {results[results.length] = data[i];
                 backup(ol)}}};
  return results}

function testalls (x,pl,al,facts,rules)
 {if (pl.length===0) {return seq(pluug(x,al,al))};
  var results = seq();
  var data = testall(pl[0],al,facts,rules);
  for (var i=0; i<data.length; i++)      {var ol = seq();
       if (maatchify(pl[0],al,data[i],al,ol))
          {results = results.concat(testalls(x,tail(pl),al,facts,rules));
           backup(ol)}};
  return results}

function compiledp (rel)
 {return (typeof window['comp' + rel]==='function')}

//------------------------------------------------------------------------------
// special relations and operators
//------------------------------------------------------------------------------

var builtins = 
 ["matches","submatches","plus","minus","times","quotient",
  "symbolize","newsymbolize",
  "readstring","stringify","readstringall","stringifyall",
  "stringappend","stringmin",
  "append","listify","delistify"];

var listoperators = 
 ["maximum","minimum","range","midrange","sum","median","mean","variance","stddev"];

var aggregates = ["avgofall", "countofall", "setofall", "sumofall"];

function updateop (x) {return findq(x,["pos", "neg"])}
function builtinp (x) {return findq(x,builtins)}
function mathp (x) {return (typeof Math[x]==='function')}
function listop (x) {return findq(x,listoperators)}function aggregatep (x) {return findq(x,aggregates)}
//------------------------------------------------------------------------------// compfindp// compfindx// compfinds//------------------------------------------------------------------------------function compfindp (query,facts,rules) {return (compfindx('true',query,facts,rules)==='true')}function compfindx (result,query,facts,rules) {return compone(result,query,seq(),seq(),nil,facts,rules)}function compfinds (result,query,facts,rules) {var answers = seq();  compall(result,query,seq(),seq(),nil,answers,facts,rules);  return uniquify(answers)}

function compfindn (m,n,result,query,facts,rules)
 {var results = sortfinds(result,query,facts,rules);
  if (results.length>=n) {return results.slice(m,n)};
  if (results.length>=m) {return results.slice(m)};
  return seq()}

function sortfinds (result,query,facts,rules) {var answers = seq();  compall(result,query,seq(),seq(),nil,answers,facts,rules);  return vniquify(answers)}
//------------------------------------------------------------------------------

function compone (x,p,pl,al,cont,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return componeatom(x,p,pl,al,cont,facts,rules)}  if (p[0]==='same') {return componesame(x,p,pl,al,cont,facts,rules)}  if (p[0]==='distinct') {return componedistinct(x,p,pl,al,cont,facts,rules)}  if (p[0]==='matches') {return componematches(x,p,pl,al,cont,facts,rules)}  if (p[0]==='submatches') {return componesubmatches(x,p,pl,al,cont,facts,rules)}
  if (builtinp(p[0])) {return componecall(x,p,pl,al,cont,facts,rules)}  if (mathp(p[0])) {return componemath(x,p,pl,al,cont,facts,rules)}
  if (listop(p[0])) {return componelist(x,p,pl,al,cont,facts,rules)}  if (p[0]==='map') {return componemap(x,p,pl,al,cont,facts,rules)}  if (p[0]==='setofall') {return componesetofall(x,p,pl,al,cont,facts,rules)}  if (p[0]==='countofall') {return componecountofall(x,p,pl,al,cont,facts,rules)}  if (p[0]==='sumofall') {return componesumofall(x,p,pl,al,cont,facts,rules)}  if (p[0]==='avgofall') {return componeavgofall(x,p,pl,al,cont,facts,rules)}  if (p[0]==='provable') {return compone(x,p[1],pl,al,cont,facts,rules)}  if (p[0]==='not') {return componenot(x,p,pl,al,cont,facts,rules)}  if (p[0]==='and') {return componeand(x,p,pl,al,cont,facts,rules)}  if (p[0]==='or') {return componeor(x,p,pl,al,cont,facts,rules)}  if (pseudogroundp(p,al)) {return componeground(x,p,pl,al,cont,facts,rules)};  return componedb(x,p,pl,al,cont,facts,rules)}

function componeatom (x,p,pl,al,cont,facts,rules) {if (p==='true') {return componeexit(x,pl,al,cont,facts,rules)};  if (p==='false') {return false};
  return componedb(x,p,pl,al,cont,facts,rules)}function componesame (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {if (answer = componeexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};  return false}function componedistinct (x,p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return componeexit(x,pl,al,cont,facts,rules)}

function componematches (x,p,pl,al,cont,facts,rules) {var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return componeexit(x,pl,al,cont,facts,rules)};
  return false}
function componesubmatches (x,p,pl,al,cont,facts,rules) {var str = pluug(p[1],al,al);
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var ol = seq();
               var result = '"' + matches[i] + '"';               if (vnifyp(p[3],al,result,al,ol))                  {if (answer = componeexit(x,pl,al,cont,facts,rules))                      {backup(ol); return answer};
                   backup(ol)}}}};  return false}
function componecall (x,p,pl,al,cont,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = componeexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}

function componemath (x,p,pl,al,cont,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = componeexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}

function componelist (x,p,pl,al,cont,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();
  var answer;  if (vnifyp(p[2],al,val,al,ol))
     {if (answer = componeexit(x,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}

function componemap (x,p,pl,al,cont,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {var answer = componeexit(x,pl,al,cont,facts,rules);
      backup(ol);
      return answer};
  return false}

function componesetofall (x,p,pl,al,cont,facts,rules) {p = pluug(p,al,al);
  var ol = seq();
  var answer;
  var result = listify(compfinds(p[1],p[2],facts,rules));
  if (vnifyp(p[3],al,result,al,ol))
     {if (answer = componeexit(x,pl,al,cont,facts,rules))         {backup(ol); return answer};
      backup(ol)};  return false}

function componecountofall (x,p,pl,al,cont,facts,rules) {p = pluug(p,al,al);
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {var answer = componeexit(x,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function componesumofall (x,p,pl,al,cont,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  compall(vars,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = componeexit(x,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function componeavgofall (x,p,pl,al,cont,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  compall(vars,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  for (var i=0; i<answers.length; i++) {total = total+numberize(answers[i][0]*1)};
  var result = total / answers.length;
  var ol = seq();
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = componeexit(x,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function componenot (x,p,pl,al,cont,facts,rules) {if (compone(x,p[1],seq(),al,nil,facts,rules)===false)
     {return componeexit(x,pl,al,cont,facts,rules)};
  return false}function componeand (x,p,pl,al,cont,facts,rules) {return componeexit(x,concatenate(tail(p),pl),al,cont,facts,rules)}function componeor (x,p,pl,al,cont,facts,rules) {var answer;
  for (var i=1; i<p.length; i++)      {if (answer = compone(x,p[i],pl,al,cont,facts,rules)) {return answer}};  return false}

function componeground (x,p,pl,al,cont,facts,rules) {if (componedb(x,p,seq(),al,nil,facts,rules))
     {return componeexit(x,pl,al,cont,facts,rules)};
  return false}function componedb (x,p,pl,al,cont,facts,rules)
 {var answer;
  if (answer = componebackground(x,p,pl,al,cont,facts,rules)) {return answer};  if (answer = componers(x,p,pl,al,cont,facts,rules)) {return answer};
  return false}function componebackground (x,p,pl,al,cont,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer;       if (vnifyp(data[i],bl,p,al,ol))          {if (answer = componeexit(x,pl,al,cont,facts,rules))
              {backup(ol); return answer};           backup(ol)}};  return false}function componers (x,p,pl,al,cont,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pl,al,cont),cont);               if (answer = componeexit(x,ql,bl,nc,facts,rules))                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = componeexit(x,pl,al,cont,facts,rules))                    {backup(ol); return answer};                 backup(ol)}}}  return false}function componeexit (x,pl,al,cont,facts,rules) {if (pl.length!==0) {return compone(x,pl[0],tail(pl),al,cont,facts,rules)};  if (nullp(cont)) {return pluug(x,al,al)};  return componeexit(x,car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}//------------------------------------------------------------------------------function compall (x,p,pl,al,cont,results,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return compallatom(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='same') {return compallsame(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='distinct') {return compalldistinct(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='matches') {return compallmatches(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='submatches') {return compallsubmatches(x,p,pl,al,cont,results,facts,rules)}  if (builtinp(p[0])) {return compallcall(x,p,pl,al,cont,results,facts,rules)}  if (mathp(p[0])) {return compallmath(x,p,pl,al,cont,results,facts,rules)}
  if (listop(p[0])) {return compalllist(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='map') {return compallmap(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='setofall') {return compallsetofall(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='countofall') {return compallcountofall(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='sumofall') {return compallsumofall(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='avgofall') {return compallavgofall(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='provable') {return compall(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='not') {return compallnot(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='and') {return compalland(x,p,pl,al,cont,results,facts,rules)}  if (p[0]==='or') {return compallor(x,p,pl,al,cont,results,facts,rules)}  if (pseudogroundp(p,al)) {return compallground(x,p,pl,al,cont,results,facts,rules)}  return compalldb(x,p,pl,al,cont,results,facts,rules)}
function compallatom (x,p,pl,al,cont,results,facts,rules) {if (p==='true') {return compallexit(x,pl,al,cont,results,facts,rules)};  if (p==='false') {return false};  return compallground(x,p,pl,al,cont,results,facts,rules)}function compallsame (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)}}function compalldistinct (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return compallexit(x,pl,al,cont,results,facts,rules)}function compallmatches (x,p,pl,al,cont,results,facts,rules) {var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return compallexit(x,pl,al,cont,results,facts,rules)};
  return false}function compallsubmatches (x,p,pl,al,cont,results,facts,rules) {var str = pluug(p[1],al,al)
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var result = '"' + matches[i] + '"';               var ol = seq();               if (vnifyp(p[3],al,result,al,ol))                  {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)}}}};  return false}function compallcall (x,p,pl,al,cont,results,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compallmath (x,p,pl,al,cont,results,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compalllist (x,p,pl,al,cont,results,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();  if (vnifyp(p[2],al,val,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compallmap (x,p,pl,al,cont,results,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compallsetofall (x,p,pl,al,cont,results,facts,rules) {p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = listify(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function compallcountofall (x,p,pl,al,cont,results,facts,rules) {p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function compallsumofall (x,p,pl,al,cont,results,facts,rules)
 {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  compall(vars,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};
  return false}

function compallavgofall (x,p,pl,al,cont,results,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  compall(vars,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  for (var i=0; i<answers.length; i++) {total = total+numberize(answers[i][0])};
  var result = total / answers.length;
  var ol = seq();
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function compallnot (x,p,pl,al,cont,results,facts,rules) {if (compone(x,p[1],seq(),al,nil,facts,rules)==false)
     {compallexit(x,pl,al,cont,results,facts,rules)}}function compalland (x,p,pl,al,cont,results,facts,rules) {compallexit(x,concatenate(tail(p),pl),al,cont,results,facts,rules)}function compallor (x,p,pl,al,cont,results,facts,rules) {for (var i=1; i<p.length; i++)
      {compall(x,p[i],pl,al,cont,results,facts,rules)}}

function compallground (x,p,pl,al,cont,results,facts,rules) {if (componedb(x,p,seq(),al,nil,facts,rules))
     {return compallexit(x,pl,al,cont,results,facts,rules)};
  return false}

function compalldb (x,p,pl,al,cont,results,facts,rules)
 {compallbackground(x,p,pl,al,cont,results,facts,rules);  compallrs(x,p,pl,al,cont,results,facts,rules);
  return false}
function compallbackground (x,p,pl,al,cont,results,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {compallexit(x,pl,al,cont,results,facts,rules);           backup(ol)}}}function compallrs (x,p,pl,al,cont,results,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pl,al,cont),cont);               compallexit(x,ql,bl,nc,results,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {compallexit(x,pl,al,cont,results,facts,rules);                 backup(ol)}}}}function compallexit (x,pl,al,cont,results,facts,rules) {if (pl.length!==0) {return compall(x,pl[0],tail(pl),al,cont,results,facts,rules)};  if (nullp(cont)) {results.push(pluug(x,al,al)); return true};  return compallexit(x,car(cont)[0],car(cont)[1],car(cont)[2],results,facts,rules)}//------------------------------------------------------------------------------
function plus ()
 {var result = 0;  for (var i=0; i<arguments.length; i++)
      {var arg = numberize(arguments[i]);
       if (isNaN(arg)) {return false};
       result = result + arg};
  return stringize(result)}

function minus ()
 {if (arguments.length===0) {return 0};
  var result = numberize(arguments[0]);  for (var i=1; i<arguments.length; i++)
      {var arg = numberize(arguments[i]);
       if (isNaN(arg)) {return false};
       result = result - arg};
  return stringize(result)}

function times ()
 {var result = 1;  for (var i=0; i<arguments.length; i++)
      {var arg = numberize(arguments[i]);
       if (isNaN(arg)) {return false};
       result = result * arg};
  return stringize(result)}

function quotient ()
 {var result = numberize(arguments[0]);  for (var i=1; i<arguments.length; i++)
      {var arg = numberize(arguments[i]);
       if (isNaN(arg)) {return false};
       result = result / arg};
  return stringize(result)}

//------------------------------------------------------------------------------
function maximum (s)
 {return Math.max.apply(null,s)}

function minimum (s)
 {return Math.min.apply(null,s)}

function range (s)
 {return maximum(s)-minimum(s)}

function midrange (s)
 {return range(s)/2}

function sum (s) 
 {var num = 0;
  for (var i=0; i<s.length; i++) {num += s[i]};
  return num}

function median (s)
 {s.sort(function(a, b) {return a - b});
  var mid = s.length/2;
  return mid%1 ? s[mid-0.5] : (s[mid-1] + s[mid])/2}

function mean (s)
 {return sum(s)/s.length}

function variance (s)
 {var avg = mean(s);
  return mean(s.map(function(num) {return Math.pow(num-avg,2)}))}

function stddev (s)
 {return Math.sqrt(variance(s))}

//------------------------------------------------------------------------------

function numberize (s)
 {if (s==='blank') {return 0};
  if (s==='false') {return 0};
  if (s==='true') {return 1};
  if (s==='infinity') {return Infinity};
  if (s==='neginfinity') {return -Infinity};
  return parseFloat(s)}

function stringize (s)
 {if (s===Infinity) {return 'infinity'};
  if (s===-Infinity) {return 'neginfinity'};
  return s + ''}

function symbolize (s)
 {s = s.replace(/[^a-z0-9]/gi,'');
  return s.toLowerCase()}

function newsymbolize (s)
 {s = replacediacritics(s);
  s = s.replace(/ /gi,'_');
  s = s.replace(/[^a-z_0-9]/gi,'');
  return s.toLowerCase()}

function replacediacritics(s)
 {var s;
  var diacritics = [
        /[\300-\306]/g, /[\340-\346]/g,  // A, a
        /[\310-\313]/g, /[\350-\353]/g,  // E, e
        /[\314-\317]/g, /[\354-\357]/g,  // I, i
        /[\322-\330]/g, /[\362-\370]/g,  // O, o
        /[\331-\334]/g, /[\371-\374]/g,  // U, u
        /[\321]/g, /[\361]/g, // N, n
        /[\307]/g, /[\347]/g, // C, c
    ];
  var chars = ['A','a','E','e','I','i','O','o','U','u','N','n','C','c'];
  for (var i = 0; i < diacritics.length; i++)
      {s = s.replace(diacritics[i],chars[i])};
  return s}

function stringmatchp (str,pat)
 {if (!stringp(str)) {return false};
  if (!stringp(pat)) {return false};
  str = str.slice(1,-1);
  pat = new RegExp(pat.slice(1,-1),'g');
  return pat.test(str)}
function stringappend ()
 {var exp='';  for (var i=0; i<arguments.length; i++) {exp += arguments[i].slice(1,-1)};  return '"' + exp + '"'}function stringify (x)
 {return '"' + grind(x) + '"'}

function stringifyall (x)
 {return '"' + stringifyallexp(x) + '"'}

function stringifyallexp (x)
 {if (x===nil) {return ''};
  if (symbolp(x)) {return ''};
  if (x[0]==='cons')
     {return grind(x[1]) + '\r' + stringifyallexp(x[2])};
  return ''}

function readstring (x)
 {return read(stripquotes(x))}

function readstringall (x)
 {return listify(readdata(stripquotes(x)))}

function stripquotes (x)
 {if (x[0]==='"' && x[x.length-1]==='"') {return x.slice(1,-1)};
  return x}

function quotify (x)
 {return '"'.concat(x).concat('"')}

function stringmin (x,y)
 {if (y<x) {return y} else {return x}}

//------------------------------------------------------------------------------function listp (x)
 {if (x===nil) {return true};
  if (symbolp(x)) {return false};
  if (x[0]==='cons') {return listp(x[2])};
  return false}

function append (l1,l2) {if (nullp(l1)) {return l2};
  if (symbolp(l1)) {return false};
  if (l1[0]!=='cons') {return false};
  return seq('cons',l1[1],append(l1[2],l2))}

function map (f,l,facts,rules)
 {if (l===nil) {return nil};
  if (symbolp(l) || l[0]!=='cons') {return false};
  var result = compfindx('Y',seq(f,l[1],'Y'),facts,rules);
  if (result===false) {return false};
  var results = map(f,l[2],facts,rules);
  if (results===false) {return false};
  return seq('cons',result,results)}

function listify (s)
 {var exp = nil;
  for (var i=s.length-1; i>=0; i--)
      {exp = seq('cons',s[i],exp)};
  return exp}function numlistify (c)
 {var s = seq();
  while (true)
   {if (c===nil) {return s};
    if (symbolp(c)) {return false};
    if (c[0]!=='cons') {return false};
    var arg = numberize(c[1]);
    if (isNaN(arg)) {return false};
    s[s.length] = arg;
    c = c[2]};
  return false}function delistify (c)
 {var s = seq();
  while (true)
   {if (c===nil) {return s};
    if (symbolp(c)) {return false};
    if (c[0]!=='cons') {return false};
    s[s.length] = c[1];
    c = c[2]};
  return false}//------------------------------------------------------------------------------

function transform (conditions,additions,deletions,facts,rules)
 {var condition = maksand(conditions);
  var adds = seq();
  var dels = seq();
  for (var i=0; i<additions.length; i++)
      {adds = adds.concat(compfinds(additions[i],condition,facts,rules))};
  for (var i=0; i<deletions.length; i++)
      {dels = dels.concat(compfinds(deletions[i],condition,facts,rules))};
  for (var i=0; i<dels.length; i++) {dropfact(dels[i],facts)};
  for (var i=0; i<adds.length; i++) {savefact(adds[i],facts)};
  return true}

//------------------------------------------------------------------------------

function compupdate (facts,rules)
 {var adds = compadditions(facts,rules);
  var dels = compdeletions(facts,rules);
  for (var i=0; i<dels.length; i++) {dropfact(dels[i],facts)};
  for (var i=0; i<adds.length; i++) {savefact(adds[i],facts)};
  return true}

function compadditions (facts,rules)
 {var adds = seq();
  var data = rules; // indexees('transition',rules);
  for (var i=0; i<data.length; i++)
      {var rule = data[i];
       if (!symbolp(rule) && rule[0]==='transition')
          {adds = adds.concat(ruleadditions(data[i][1],data[i][2],facts,rules))}};
  return adds}

function ruleadditions (condition,conclusion,facts,rules)
 {if (symbolp(conclusion)) {return compfinds(conclusion,condition,facts,rules)};
  if (conclusion[0]==='not') {return seq()};
  if (conclusion[0]==='and')
     {var adds = seq();
      for (var i=1; i<conclusion.length; i++)
          {var answers = ruleadditions(condition,conclusion[i],facts,rules);
           adds = adds.concat(answers)};
      return adds};
  return compfinds(conclusion,condition,facts,rules)}

function compdeletions (facts,rules)
 {var dels = seq();
  var data = rules; // indexees('transition',rules);
  for (var i=0; i<data.length; i++)
      {var rule = data[i];
       if (!symbolp(rule) && rule[0]==='transition')
          {dels = dels.concat(ruledeletions(data[i][1],data[i][2],facts,rules))}};
  return dels}

function ruledeletions (condition,conclusion,facts,rules)
 {if (symbolp(conclusion)) {return seq()};
  if (conclusion[0]==='not')
     {return compfinds(conclusion[1],condition,facts,rules)};
  if (conclusion[0]==='and')
     {var dels = seq();
      for (var i=1; i<conclusion.length; i++)
          {var answers = ruledeletions(condition,conclusion[i],facts,rules);
           dels = dels.concat(answers)};
      return dels};
  return seq()}

//------------------------------------------------------------------------------// hypofindp// hypofindx// hypofinds//------------------------------------------------------------------------------

function hypofindp (query,adds,dels,facts,rules) {return hypofindx('true',query,adds,dels,facts,rules)}function hypofindx (result,query,adds,dels,facts,rules) {return hypoone(result,query,seq(),seq(),nil,adds,dels,facts,rules)}function hypofinds (result,query,adds,dels,facts,rules) {var answers = seq();  hypoall(result,query,seq(),seq(),nil,answers,adds,dels,facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------

function hypoone (x,p,pl,al,cont,adds,dels,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return hypooneatom(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='same') {return hypoonesame(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='distinct') {return hypoonedistinct(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='matches') {return hypoonematches(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='submatches') {return hypoonesubmatches(x,p,pl,al,cont,adds,dels,facts,rules)}  if (builtinp(p[0])) {return hypoonecall(x,p,pl,al,cont,adds,dels,facts,rules)}  if (mathp(p[0])) {return hypoonemath(x,p,pl,al,cont,adds,dels,facts,rules)}  if (listop(p[0])) {return hypoonelist(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='map') {return hypoonemap(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='setofall') {return hypoonesetofall(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='countofall') {return hypoonecountofall(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='sumofall') {return hypoonesumofall(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='avgofall') {return hypooneavgofall(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='provable') {return hypoone(x,p[1],pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='not') {return hypoonenot(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='and') {return hypooneand(x,p,pl,al,cont,adds,dels,facts,rules)}  if (p[0]==='or') {return hypooneor(x,p,pl,al,cont,adds,dels,facts,rules)}  if (pseudogroundp(p,al)) {return hypooneground(x,p,pl,al,cont,adds,dels,facts,rules)};  return hypoonedb(x,p,pl,al,cont,adds,dels,facts,rules)}

function hypooneatom (x,p,pl,al,cont,adds,dels,facts,rules) {if (p==='true') {return hypooneexit(x,pl,al,cont,adds,dels,facts,rules)};  if (p==='false') {return false};
  return hypoonedb(x,p,pl,al,cont,adds,dels,facts,rules)}function hypoonesame (x,p,pl,al,cont,adds,dels,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))
         {backup(ol); return answer};
      backup(ol)};  return false}function hypoonedistinct (x,p,pl,al,cont,adds,dels,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return hypooneexit(x,pl,al,cont,adds,dels,facts,rules)}

function hypoonematches (x,p,pl,al,cont,adds,dels,facts,rules) {var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return hypooneexit(x,pl,al,cont,adds,dels,facts,rules)};
  return false}
function hypoonesubmatches (x,p,pl,al,cont,adds,dels,facts,rules) {var str = pluug(p[1],al,al);
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var ol = seq();
               var result = '"' + matches[i] + '"';               if (vnifyp(p[3],al,result,al,ol))                  {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))                      {backup(ol); return answer};
                   backup(ol)}}}};  return false}
function hypoonecall (x,p,pl,al,cont,adds,dels,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))
         {backup(ol); return answer}};
  return false}

function hypoonemath (x,p,pl,al,cont,adds,dels,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))
         {backup(ol); return answer}};
  return false}

function hypoonelist (x,p,pl,al,cont,adds,dels,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();
  var answer;  if (vnifyp(p[2],al,val,al,ol))
     {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))
         {backup(ol); return answer}};
  return false}

function hypoonemap (x,p,pl,al,cont,adds,dels,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {var answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules);
      backup(ol);
      return answer};
  return false}

function hypoonesetofall (x,p,pl,al,cont,adds,dels,facts,rules) {p = pluug(p,al,al);
  var result = listify(hypofinds(p[1],p[2],adds,dels,facts,rules));
  var ol = seq();
  if (vnifyp(p[3],al,result,al,ol))
     {var answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules);      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function hypoonecountofall (x,p,pl,al,cont,adds,dels,facts,rules) {p = pluug(p,al,al);
  var answers = seq();
  hypoall(p[1],p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {var answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules)
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function hypoonesumofall (x,p,pl,al,cont,adds,dels,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  hypoall(vars,p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function hypooneavgofall (x,p,pl,al,cont,adds,dels,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  hypoall(vars,p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {total = total+answers[i][0]*1};
  var result = total / answers.length;
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function hypoonenot (x,p,pl,al,cont,adds,dels,facts,rules) {if (hypoone(x,p[1],seq(),al,nil,adds,dels,facts,rules)==false)
     {return hypooneexit(x,pl,al,cont,adds,dels,facts,rules)};
  return false}function hypooneand (x,p,pl,al,cont,adds,dels,facts,rules) {return hypooneexit(x,concatenate(tail(p),pl),al,cont,adds,dels,facts,rules)}function hypooneor (x,p,pl,al,cont,adds,dels,facts,rules) {var answer;
  for (var i=1; i<p.length; i++)      {if (answer = hypoone(x,p[i],pl,al,cont,adds,dels,facts,rules)) {return answer}};  return false}

function hypooneground (x,p,pl,al,cont,adds,dels,facts,rules) {if (hypoonedb(x,p,seq(),al,nil,adds,dels,facts,rules))
     {return hypooneexit(x,pl,al,cont,adds,dels,facts,rules)};
  return false}function hypoonedb (x,p,pl,al,cont,adds,dels,facts,rules)
 {var answer;
  if (answer = hypoonebackground(x,p,pl,al,cont,adds,dels,facts,rules)) {return answer};  if (answer = hypooners(x,p,pl,al,cont,adds,dels,facts,rules)) {return answer};
  return false}function hypoonebackground (x,p,pl,al,cont,adds,dels,facts,rules) {for (var i=0; i<adds.length; i++)      {var bl = seq();       var ol = seq();       var answer = false;       if (vnifyp(adds[i],bl,p,al,ol))          {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))
              {backup(ol); return answer};           backup(ol)}};
  var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (vnifyp(data[i],bl,p,al,ol))          {if (!find(data[i],dels) && (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules)))
              {backup(ol); return answer};           backup(ol)}};  return false}function hypooners (x,p,pl,al,cont,adds,dels,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               if (answer = hypoone(x,data[i][2],ql,bl,nc,adds,dels,facts,rules))                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = hypooneexit(x,pl,al,cont,adds,dels,facts,rules))                    {backup(ol); return answer};                 backup(ol)}}}  return false}function hypooneexit (x,pl,al,cont,adds,dels,facts,rules) {if (pl.length!==0) {return hypoone(x,pl[0],tail(pl),al,cont,adds,dels,facts,rules)};  if (nullp(cont)) {return pluug(x,al,al)};  return hypooneexit(x,car(cont)[0],car(cont)[1],car(cont)[2],adds,dels,facts,rules)}//------------------------------------------------------------------------------function hypoall (x,p,pl,al,cont,results,adds,dels,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return hypoallatom(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='same') {return hypoallsame(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='distinct') {return hypoalldistinct(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='matches') {return hypoallmatches(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='submatches') {return hypoallsubmatches(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (builtinp(p[0])) {return hypoallcall(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (mathp(p[0])) {return hypoallmath(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (listop(p[0])) {return hypoalllist(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='map') {return hypoallmap(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='setofall') {return hypoallsetofall(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='countofall') {return hypoallcountofall(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='sumofall') {return hypoallsumofall(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='avgofall') {return hypoallavgofall(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='provable') {return hypoall(x,p[1],pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='not') {return hypoallnot(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='and') {return hypoalland(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (p[0]==='or') {return hypoallor(x,p,pl,al,cont,results,adds,dels,facts,rules)}  if (pseudogroundp(p,al)) {return hypoallground(x,p,pl,al,cont,results,adds,dels,facts,rules)}  return hypoalldb(x,p,pl,al,cont,results,adds,dels,facts,rules)}
function hypoallatom (x,p,pl,al,cont,results,adds,dels,facts,rules) {if (p==='true') {return hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)};  if (p==='false') {return false};  return hypoalldb(x,p,pl,al,cont,results,adds,dels,facts,rules)}function hypoallsame (x,p,pl,al,cont,results,adds,dels,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)}}function hypoalldistinct (x,p,pl,al,cont,results,adds,dels,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)}function hypoallmatches (x,p,pl,al,cont,results,adds,dels,facts,rules) {var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)};
  return false}function hypoallsubmatches (x,p,pl,al,cont,results,adds,dels,facts,rules) {var str = pluug(p[1],al,al)
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var result = '"' + matches[i] + '"';               var ol = seq();               if (vnifyp(p[3],al,result,al,ol))                  {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)}}}};  return false}function hypoallcall (x,p,pl,al,cont,results,adds,dels,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)};
  return false}

function hypoallmath (x,p,pl,al,cont,results,adds,dels,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)};
  return false}

function hypoalllist (x,p,pl,al,cont,results,adds,dels,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();  if (vnifyp(p[2],al,val,al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)};
  return false}

function hypoallmap (x,p,pl,al,cont,results,adds,dels,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules); backup(ol)};
  return false}

function hypoallsetofall (x,p,pl,al,cont,results,adds,dels,facts,rules) {p = pluug(p,al,al);
  var answers = seq();
  hypoall(p[1],p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var result = listify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,result,al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);
      backup(ol);
      return false};  return false}

function hypoallcountofall (x,p,pl,al,cont,results,adds,dels,facts,rules) {p = pluug(p,al,al);
  var answers = seq();
  hypoall(p[1],p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);
      backup(ol);
      return false};  return false}

function hypoallsumofall (x,p,pl,al,cont,results,adds,dels,facts,rules)
 {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  hypoall(vars,p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);
      backup(ol);
      return false};
  return false}

function hypoallavgofall (x,p,pl,al,cont,results,adds,dels,facts,rules) {p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  hypoall(vars,p[2],seq(),al,nil,answers,adds,dels,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {total = total+answers[i][0]*1};
  var result = total / answers.length;
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);
      backup(ol);
      return false};  return false}

function hypoallnot (x,p,pl,al,cont,results,adds,dels,facts,rules) {if (hypoone(x,p[1],seq(),al,nil,adds,dels,facts,rules)==false)
     {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)}}function hypoalland (x,p,pl,al,cont,results,adds,dels,facts,rules) {hypoallexit(x,concatenate(tail(p),pl),al,cont,results,adds,dels,facts,rules)}function hypoallor (x,p,pl,al,cont,results,adds,dels,facts,rules) {for (var i=1; i<p.length; i++)
      {hypoall(x,p[i],pl,al,cont,results,adds,dels,facts,rules)}}

function hypoallground (x,p,pl,al,cont,results,adds,dels,facts,rules) {if (hypoonedb(x,p,seq(),al,nil,adds,dels,facts,rules))
     {return hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)};
  return false}

function hypoalldb (x,p,pl,al,cont,results,adds,dels,facts,rules)
 {hypoallbackground(x,p,pl,al,cont,results,adds,dels,facts,rules);  hypoallrs(x,p,pl,al,cont,results,adds,dels,facts,rules);
  return false}
function hypoallbackground (x,p,pl,al,cont,results,adds,dels,facts,rules) {for (var i=0; i<adds.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(adds[i],bl,p,al,ol))          {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);           backup(ol)}};
  var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {if (!find(data[i],dels)) {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules)};           backup(ol)}}}function hypoallrs (x,p,pl,al,cont,results,adds,dels,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               hypoall(x,data[i][2],ql,bl,nc,results,adds,dels,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {hypoallexit(x,pl,al,cont,results,adds,dels,facts,rules);                 backup(ol)}}}}function hypoallexit (x,pl,al,cont,results,adds,dels,facts,rules) {if (pl.length!==0) {return hypoall(x,pl[0],tail(pl),al,cont,results,adds,dels,facts,rules)};  if (nullp(cont)) {results.push(pluug(x,al,al)); return true};  return hypoallexit(x,car(cont)[0],car(cont)[1],car(cont)[2],results,adds,dels,facts,rules)}

//------------------------------------------------------------------------------function hypoupdate (adds,dels,facts,rules)
 {var additions = hypoadditions(adds,dels,facts,rules);
  var deletions = hypodeletions(adds,dels,facts,rules);
  for (var i=0; i<deletions.length; i++) {dropfact(deletions[i],facts)};
  for (var i=0; i<additions.length; i++) {savefact(additions[i],facts)};
  return true}

function hypoadditions (adds,dels,facts,rules)
 {var additions = seq();
  var data = rules; // indexees('transition',rules);
  for (var i=0; i<data.length; i++)
      {var rule = data[i];
       if (!symbolp(rule) && rule[0]==='transition')
          {additions = additions.concat(hyporuleadditions(data[i][1],data[i][2],adds,dels,facts,rules))}};
  return additions}

function hyporuleadditions (condition,conclusion,adds,dels,facts,rules)
 {if (symbolp(conclusion))
     {return hypofinds(conclusion,condition,adds,dels,facts,rules)};
  if (conclusion[0]==='not') {return seq()};
  if (conclusion[0]==='and')
     {var additions = seq();
      for (var i=1; i<conclusion.length; i++)
          {var answers = 
               hyporuleadditions(condition,conclusion[i],adds,dels,facts,rules);
           additions = additions.concat(answers)};
      return additions};
  return hypofinds(conclusion,condition,adds,dels,facts,rules)}

function hypodeletions (adds,dels,facts,rules)
 {var deletions = seq();
  var data = rules; // indexees('transition',rules);
  for (var i=0; i<data.length; i++)
      {var rule = data[i];
       if (!symbolp(rule) && rule[0]==='transition')
          {deletions = deletions.concat(hyporuledeletions(data[i][1],data[i][2],adds,dels,facts,rules))}};
  return deletions}

function hyporuledeletions (condition,conclusion,adds,dels,facts,rules)
 {if (symbolp(conclusion)) {return seq()};
  if (conclusion[0]==='not')
     {return hypofinds(conclusion[1],condition,adds,dels,facts,rules)};
  if (conclusion[0]==='and')
     {var deletions = seq();
      for (var i=1; i<conclusion.length; i++)
          {var answers = 
               hyporuledeletions(condition,conclusion[i],adds,dels,facts,rules);
           deletions = deletions.concat(answers)};
      return deletions};
  return seq()}

//------------------------------------------------------------------------------// tempfindp// tempfindx// tempfinds//------------------------------------------------------------------------------
function tempfindp (query,temprules,facts,rules) {return tempfindx('true',query,temprules,facts,rules)}function tempfindx (result,query,temprules,facts,rules) {for (var i=0; i<temprules.length; i++) {insertrule(temprules[i],rules)};
  var dum = compfindx(result,query,facts,rules);
  for (var i=0; i<temprules.length; i++) {uninsertrule(temprules[i],rules)};
  return dum}function tempfinds (result,query,temprules,facts,rules) {for (var i=0; i<temprules.length; i++) {insertrule(temprules[i],rules)};
  var answers = sortfinds(result,query,facts,rules);  for (var i=0; i<temprules.length; i++) {uninsertrule(temprules[i],rules)};
  return answers}

//------------------------------------------------------------------------------// tracefindp// tracefindx// tracefinds//------------------------------------------------------------------------------function tracefindp (query,facts,rules) {return (tracefindx('true',query,facts,rules)==='true')}function tracefindx (result,query,facts,rules) {var xl = seq();
  return traceone(result,xl,query,seq(),xl,nil,facts,rules)}function tracefinds (result,query,facts,rules) {var xl = seq();
  var answers = seq();  traceall(result,xl,query,seq(),xl,nil,answers,facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------function traceone (x,xl,p,pl,al,cont,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return traceoneatom(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='same') {return traceonesame(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='distinct') {return traceonedistinct(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='matches') {return traceonematches(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='submatches') {return traceonesubmatches(x,xl,p,pl,al,cont,facts,rules)}  if (builtinp(p[0])) {return traceonecall(x,xl,p,pl,al,cont,facts,rules)}  if (mathp(p[0])) {return traceonemath(x,xl,p,pl,al,cont,facts,rules)}  if (listop(p[0])) {return traceonelist(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='map') {return traceonemap(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='setofall') {return traceonesetofall(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='countofall') {return traceonecountofall(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='sumofall') {return traceonesumofall(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='avgofall') {return traceoneavgofall(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='provable') {return traceone(x,xl,p[1],pl,al,cont,facts,rules)}  if (p[0]==='not') {return traceonenot(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='and') {return traceoneand(x,xl,p,pl,al,cont,facts,rules)}  if (p[0]==='or') {return traceoneor(x,xl,p,pl,al,cont,facts,rules)}  //if (pseudogroundp(p,al)) {return traceoneground(x,xl,p,pl,al,cont,facts,rules)};  return traceonedb(x,xl,p,pl,al,cont,facts,rules)}

function traceoneatom (x,xl,p,pl,al,cont,facts,rules) {if (p==='true')
     {tracecall(p,al,xl,cont);
      traceexit(p,xl,xl,cont);
      return traceonegoals(x,xl,p,pl,al,cont,facts,rules)};  if (p==='false')
     {tracecall(p,al,xl,cont);
      tracefail(p,al,xl,cont);
      return false};
  return traceonedb(x,xl,p,pl,al,cont,facts,rules)}function traceonesame (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {answer = traceoneexit(x,xl,pluug(p,al,xl),pl,al,cont,facts,rules);
      backup(ol);
      if (answer) {return answer}};
  tracefail(p,al,xl,cont);  return false}function traceonedistinct (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {backup(ol); tracefail(p,al,xl,cont); return false};
  traceexit(p,xl,xl,cont);  return traceonegoals(x,xl,pl,al,cont,facts,rules)}

function traceonematches (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return traceoneexit(x,xl,p,pl,al,cont,facts,rules)};
  tracefail(p,al,xl,cont);
  return false}

function traceonesubmatches (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var str = pluug(p[1],al,al);
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var ol = seq();
               var result = '"' + matches[i] + '"';               if (vnifyp(p[3],al,result,al,ol))                  {if (answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules))                      {backup(ol); return answer};
                   backup(ol)}}}};
  tracefail(p,al,xl,cont);  return false}function traceonecall (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  tracefail(p,al,xl,cont);  return false}

function traceonemath (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();
  var answer;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  tracefail(p,al,xl,cont);  return false}

function traceonelist (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();
  var answer;  if (vnifyp(p[2],al,val,al,ol))
     {if (answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  tracefail(p,al,xl,cont);  return false}

function traceonemap (x,xl,p,pl,al,cont,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {var answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules);
      backup(ol);
      return answer};
  return false}

function traceonesetofall (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var ol = seq();
  var answer;
  var result = listify(tracefinds(p[1],p[2],facts,rules));
  if (vnifyp(p[3],al,result,al,ol))
     {if (answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules))         {backup(ol); return answer};
      backup(ol)};  return false}

function traceonecountofall (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var answers = seq();
  traceall(p[1],al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {var answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function traceonesumofall (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  traceall(vars,al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function traceoneavgofall (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  traceall(vars,al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  for (var i=0; i<answers.length; i++) {total = total+numberize(answers[i][0]*1)};
  var result = total / answers.length;
  var ol = seq();
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {var answer = traceoneexit(x,xl,p,pl,al,cont,facts,rules);
      if (answer) {backup(ol); return answer};
      backup(ol)};  return false}

function traceonenot (x,xl,p,pl,al,cont,facts,rules) {tracecall(p,al,xl,cont);
  if (traceone(x,xl,p[1],seq(),al,nil,facts,rules)==false)
     {return traceoneexit(x,xl,pluug(p,al,xl),pl,al,cont,facts,rules)};
  tracefail(p,al,xl,cont);  return false}function traceoneand (x,xl,p,pl,al,cont,facts,rules) {//tracecall(p,al,xl,cont);
  return traceonegoals(x,xl,p,concatenate(tail(p),pl),al,cont,facts,rules)}function traceoneor (x,xl,p,pl,al,cont,facts,rules) {//tracecall(p,al,xl,cont);
  var answer;
  for (var i=1; i<p.length; i++)      {if (answer = traceone(x,xl,p[i],pl,al,cont,facts,rules)) {return answer}};  tracefail(p,al,xl,cont);
  return false}

function traceoneground (x,xl,p,pl,al,cont,facts,rules) {if (traceonedb(x,xl,p,seq(),al,nil,facts,rules))
     {return traceonegoals(x,xl,p,pl,al,cont,facts,rules)};
  return false}

function traceonedb (x,xl,p,pl,al,cont,facts,rules)
 {tracecall(p,al,xl,cont);
  var answer;
  if (answer = traceonebackground(x,xl,p,pl,al,cont,facts,rules)) {return answer};  if (answer = traceoners(x,xl,p,pl,al,cont,facts,rules)) {return answer};
  tracefail(p,al,xl,cont);  return false}function traceonebackground (x,xl,p,pl,al,cont,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer;       if (vnifyp(data[i],bl,p,al,ol))          {if (answer = traceoneexit(x,xl,pluug(p,al,xl),pl,al,cont,facts,rules))
              {backup(ol); return answer};           backup(ol);
           traceredo(pluug(p,al,xl),al,xl,cont)}};  return false}function traceoners (x,xl,p,pl,al,cont,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pluug(p,al,xl),pl,al,cont),cont);               if (answer = traceonegoals(x,xl,data[i][1],ql,bl,nc,facts,rules))                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = traceoneexit(x,xl,pluug(p,al,xl),pl,al,cont,facts,rules))                    {backup(ol); return answer};                 backup(ol)}}}  return false}function traceonegoals (x,xl,p,pl,al,cont,facts,rules) {if (pl.length!==0) {return traceone(x,xl,pl[0],tail(pl),al,cont,facts,rules)};  if (nullp(cont)) {return pluug(x,al,xl)};  return traceoneexit(x,xl,car(cont)[0],car(cont)[1],car(cont)[2],car(cont)[3],facts,rules)}function traceoneexit (x,xl,p,pl,al,cont,facts,rules) {traceexit(p,xl,xl,cont);
  return traceonegoals(x,xl,p,pl,al,cont,facts,rules)}//------------------------------------------------------------------------------

function traceall (x,xl,p,pl,al,cont,results,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return traceallatom(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='same') {return traceallsame(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='distinct') {return tracealldistinct(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='matches') {return traceallmatches(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='submatches') {return traceallsubmatches(x,xl,p,pl,al,cont,results,facts,rules)}  if (builtinp(p[0])) {return traceallcall(x,xl,p,pl,al,cont,results,facts,rules)}  if (mathp(p[0])) {return traceallmath(x,xl,p,pl,al,cont,results,facts,rules)}  if (listop(p[0])) {return tracealllist(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='map') {return traceallmap(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='setofall') {return traceallsetofall(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='countofall') {return traceallcountofall(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='sumofall') {return traceallsumofall(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='avgofall') {return traceallavgofall(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='provable') {return traceall(x,xl,p[1],pl,al,cont,results,facts,rules)}  if (p[0]==='not') {return traceallnot(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='and') {return tracealland(x,xl,p,pl,al,cont,results,facts,rules)}  if (p[0]==='or') {return traceallor(x,xl,p,pl,al,cont,results,facts,rules)}
  //if (pseudogroundp(p,al)) {return traceallground(x,xl,p,pl,al,cont,results,facts,rules)}  tracealldb(x,xl,p,pl,al,cont,results,facts,rules);
  return false}

function traceallatom (x,xl,p,pl,al,cont,results,facts,rules) {if (p==='true')
     {tracecall(p,al,xl,cont);
      traceexit(p,xl,xl,cont);
      traceallgoals(x,xl,p,pl,al,cont,results,facts,rules)};  if (p==='false')
     {tracecall(p,al,xl,cont);
      tracefail(p,al,xl,cont);
      return false};
  tracealldb(x,xl,p,pl,al,cont,results,facts,rules);
  return false}function traceallsame (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {traceallexit(x,xl,pluug(p,al,xl),pl,al,cont,results,facts,rules); backup(ol); return false};
  tracefail(p,al,xl,cont);
  return false}function tracealldistinct (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {backup(ol); tracefail(p,al,xl,cont); return false};  traceexit(p,al,xl,cont);
  return traceallgoals(x,xl,p,pl,al,cont,results,facts,rules)}

function traceallmatches (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return traceallexit(x,xl,p,pl,al,cont,results,facts,rules)};
  tracefail(p,al,xl,cont);
  return false}

function traceallsubmatches (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var str = pluug(p[1],al,al)
  str = str.substring(1,str.length-1);
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var result = '"' + matches[i] + '"';               var ol = seq();               if (vnifyp(p[3],al,result,al,ol))                  {traceallexit(x,xl,p,pl,al,cont,results,facts,rules); backup(ol)}}}};  tracefail(p,al,xl,cont);
  return false}function traceallcall (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules); backup(ol); return false};
  tracefail(p,al,xl,cont);
  return false}

function traceallmath (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules); backup(ol); return false};
  tracefail(p,al,xl,cont);
  return false}

function tracealllist (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();  if (vnifyp(p[2],al,val,al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules); backup(ol); return false};
  tracefail(p,al,xl,cont);
  return false}

function traceallmap (x,xl,p,pl,al,cont,results,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function traceallsetofall (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  traceall(p[1],al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = listify(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function traceallcountofall (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  traceall(p[1],al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function traceallsumofall (x,xl,p,pl,al,cont,results,facts,rules)
 {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  traceall(vars,al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var result = 0;
  var ol = seq();
  for (var i=0; i<answers.length; i++) {result = result+numberize(answers[i][0])};
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};
  return false}

function traceallavgofall (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  p = pluug(p,al,al);
  var vars = freevarsexp(p[2],al,seq(p[1]));
  var answers = seq();
  traceall(vars,al,p[2],seq(),al,nil,answers,facts,rules);
  answers = vniquify(answers);
  var total = 0;
  for (var i=0; i<answers.length; i++) {total = total+numberize(answers[i][0])};
  var result = total / answers.length;
  var ol = seq();
  if (!isNaN(result) && vnifyp(p[3],al,result.toString(),al,ol))
     {traceallexit(x,xl,p,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function traceallnot (x,xl,p,pl,al,cont,results,facts,rules) {tracecall(p,al,xl,cont);
  if (traceone(x,xl,p[1],seq(),al,nil,facts,rules)==false)
     {traceallexit(x,xl,pluug(p,al,xl),pl,al,cont,results,facts,rules)};
  return false}function tracealland (x,xl,p,pl,al,cont,results,facts,rules) {//tracecall(p,al,xl,cont);
  traceallgoals(x,xl,p,concatenate(tail(p),pl),al,cont,results,facts,rules);
  return false}function traceallor (x,xl,p,pl,al,cont,results,facts,rules) {//tracecall(p,al,xl,cont);
  for (var i=1; i<p.length; i++)
      {traceall(x,xl,p[i],pl,al,cont,results,facts,rules)};
  return false}

function traceallground (x,xl,p,pl,al,cont,results,facts,rules) {if (traceonedb(x,xl,p,seq(),al,nil,facts,rules))
     {return traceallgoals(x,xl,p,pl,al,cont,results,facts,rules)};
  return false}

function tracealldb (x,xl,p,pl,al,cont,results,facts,rules)
 {tracecall(p,al,xl,cont);
  traceallbackground(x,xl,p,pl,al,cont,results,facts,rules);  traceallrs(x,xl,p,pl,al,cont,results,facts,rules);
  tracefail(p,al,xl,cont);  return false}
function traceallbackground (x,xl,p,pl,al,cont,results,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {traceallexit(x,xl,pluug(p,al,xl),pl,al,cont,results,facts,rules);           backup(ol);
           traceredo(pluug(p,al,xl),al,xl,cont)}};
  return false}

function traceallrs (x,xl,p,pl,al,cont,results,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(2);               var nc = cons(seq(pluug(p,al,xl),pl,al,cont),cont);               traceallgoals(x,xl,data[i][1],ql,bl,nc,results,facts,rules);
               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {traceallexit(x,xl,p,pl,al,cont,results,facts,rules);                 backup(ol)};
             traceredo(p,al,xl,cont)}}}

function traceallgoals (x,xl,p,pl,al,cont,results,facts,rules) {if (pl.length!==0) {return traceall(x,xl,pl[0],tail(pl),al,cont,results,facts,rules)};
  if (nullp(cont)) {results.push(pluug(x,al,xl)); return true};
  traceallexit(x,xl,car(cont)[0],car(cont)[1],car(cont)[2],car(cont)[3],results,facts,rules);
  traceredo(car(cont)[0],car(cont)[2],xl,car(cont)[3])}

function traceallexit (x,xl,p,pl,al,cont,results,facts,rules) {traceexit(p,xl,xl,cont);
  return traceallgoals(x,xl,p,pl,al,cont,results,facts,rules)}
function tracecall (p,al,xl,cont)
 {console.log(grindspaces(len(cont)) + 'Call: ' + grind(pluug(p,al,xl)))}

function traceexit (p,al,xl,cont)
 {console.log(grindspaces(len(cont)) + 'Exit: ' + grind(pluug(p,al,xl)))}

function traceredo (p,al,xl,cont)
 {console.log(grindspaces(len(cont)) + 'Redo: ' + grind(p))}

function tracefail (p,al,xl,cont)
 {console.log(grindspaces(len(cont)) + 'Fail: ' + grind(pluug(p,al,xl)))}

function grindspaces (n)
 {if (n===0) {return ''};
  return grindspaces(n-1) + '| '}

//------------------------------------------------------------------------------// viewfindp// viewfindx// viewfinds//------------------------------------------------------------------------------

function viewfindp (query,facts,rules) {return compfindp(query,facts,rules)}function viewfindx (result,query,facts,rules) {return compfindx(result,query,facts,rules)}
function viewfinds (x,p,facts,rules)
 {var answers = viewalls(x,seq(p),seq(),facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------

function viewall (p,al,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return viewallatom(p,al,facts,rules)}
  if (p[0]==='same') {return viewallsame(p,al,facts,rules)}  if (p[0]==='distinct') {return viewalldistinct(p,al,facts,rules)}  if (p[0]==='matches') {return viewallmatches(p,al,facts,rules)}  if (p[0]==='submatches') {return viewallsubmatches(p,al,facts,rules)}  if (builtinp(p[0])) {return viewallcall(p,al,facts,rules)}  if (mathp(p[0])) {return viewallmath(p,al,facts,rules)}
  if (listop(p[0])) {return viewalllist(p,al,facts,rules)}  if (p[0]==='map') {return viewallmap(p,al,facts,rules)}  if (p[0]==='setofall') {return viewallsetofall(p,al,facts,rules)}  if (p[0]==='countofall') {return viewallcountofall(p,al,facts,rules)}  if (p[0]==='provable') {return viewallprovable(p,al,facts,rules)}  if (p[0]==='not') {return viewallnot(p,al,facts,rules)}  if (p[0]==='and') {return viewalland(p,al,facts,rules)}  if (p[0]==='or') {return viewallor(p,al,facts,rules)}  if (pseudogroundp(p,al)) {return viewallground(p,al,facts,rules)}  if (!viewp(p[0],rules)) {return viewallfacts(p,al,facts,rules)}
  return viewallrules(p,al,facts,rules)}
function viewallatom (p,al,facts,rules) {if (p==='true') {return seq(p)};  if (p==='false') {return seq()};  return viewallground(p,al,facts,rules)}

function viewallsame (p,al,facts,rules) {var ol = seq();
  if (vnifyp(p[1],al,p[2],al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}
function viewalldistinct (p,al,facts,rules) {var ol = seq();
  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return seq()};  return seq(pluug(p,al,al))}

function viewallmatches (p,al,facts,rules) {var str = pluug(p[1],al,al);
  if (!stringp(str)) {return false};
  var pat = pluug(p[2],al,al);
  if (!stringp(pat)) {return false};
  if (stringmatchp(str,pat)) {return seq(seq('matches',str,pat))};
  return seq()}function viewallsubmatches (p,al,facts,rules) {var str = pluug(p[1],al,al);
  str = str.substring(1,str.length-1);
  results = seq();
  if (symbolp(str))     {var re=new RegExp(p[2].substring(1,p[2].length-1),'g');
      var matches = str.match(re);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var result = '"' + matches[i] + '"';               var ol = seq();               if (vnifyp(p[3],al,result,al,ol))                  {results.push(seq('submatches',str,p[2],result)); backup(ol)}}}};  return results}

function viewallcall (p,al,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  if (!val) {return false};
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}

function viewallmath (p,al,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}

function viewalllist (p,al,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();  if (vnifyp(p[2],al,val,al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}

function viewallmap (p,al,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();
  if (vnifyp(p[3],al,val,al,ol))
     {p = pluug(p,al,al);
      backup(ol);
      return seq(p)}
  return seq()}

function viewallsetofall (p,al,facts,rules) {p = pluug(p,al,al);
  var answers = viewfinds(p[1],p[2],facts,rules);
  var result = listify(answers);
  var ol = seq();
  if (vnifyp(p[3],al,result,al,ol))
     {p = pluug(p,al,al); backup(ol); return seq(p)};  return seq()}

function viewallcountofall (p,al,facts,rules) {p = pluug(p,al,al);
  var answers = viewfinds(p[1],p[2],facts,rules);
  var ol = seq();
  if (vnifyp(p[3],al,answers.length.toString(),al,ol))
     {p = pluug(p,al,al); backup(ol); return seq(p)};  return seq()}

function viewallprovable (p,al,facts,rules) {p = componedb(p,p[1],seq(),al,nil,facts,rules);
  if (p!==false) {return seq(p)};
  return seq()}
function viewallnot (p,al,facts,rules) {p = pluug(p,al,al);
  if (compfindp(p[1],facts,rules)) {return seq()};
  return seq(p)}function viewalland (p,al,facts,rules) {return viewalls(p,tail(p),al,facts,rules)}

function viewallor (p,al,facts,rules) {var results = seq();
  for (var i=1; i<p.length; i++)      {results = results.concat(viewalls(p,seq(p[i]),al,facts,rules))};
  return results}

function viewallground (p,al,facts,rules) {p = pluug(p,al,al);
  if (componedb('true',p,seq(),seq(),nil,facts,rules)==='true') {return seq(p)};
  return seq()}function viewallfacts (p,al,facts,rules) {return envvndexps(p,al,facts)}

function viewallrules (p,al,facts,rules) {var results = seq();  var data = viewindexps(p,rules);
  for (var i=0; i<data.length; i++)      {if (data[i][0]==='rule')          {var bl = seq();
           var ol = seq();
           if (vnifyp(p,al,data[i][1],bl,ol))              {var head = data[i][1];
               var body = data[i].slice(2);
               var answers = viewalls(head,body,bl,facts,rules);
               results = results.concat(answers);
               backup(ol)}}
       else {var ol = seq();
             if (maatchifyp(p,al,data[i],al,ol))
                {results[results.length] = data[i];
                 backup(ol)}}};
  return results}

function viewalls (x,pl,al,facts,rules)
 {if (pl.length===0) {return seq(pluug(x,al,al))};
  var results = seq();
  var data = viewall(pl[0],al,facts,rules);
  for (var i=0; i<data.length; i++)      {var ol = seq();
       if (maatchify(pl[0],al,data[i],al,ol))
          {results = results.concat(viewalls(x,tail(pl),al,facts,rules));
           backup(ol)}};
  return results}

//------------------------------------------------------------------------------
// baseresidue// baseresidues//------------------------------------------------------------------------------// Notes:
//   assumes query is ground
//   does not do negation as failure on prims (correctly)
//   does not do consistency check (correctly) - assumes constraints folded in
//------------------------------------------------------------------------------function baseresidue (query,prims,facts,rules) {var answer = baseresone(query,seq(),seq(),nil,prims,seq(),seq(),facts,rules);
  if (answer) {return maksand(answer[0])};
  return false}

function baseresidues (query,prims,facts,rules) {var answers = seq();  baseresall(query,seq(),seq(),nil,answers,prims,seq(),seq(),facts,rules);
  for (var i=0; i<answers.length; i++) {answers[i] = maksand(answers[i][0])};
  return uniquify(answers)}

//------------------------------------------------------------------------------function baseresone (p,pl,al,cont,prims,pos,neg,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return baseresoneatom(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='same') {return baseresonesame(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='distinct') {return baseresonedistinct(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='not') {return baseresonenot(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='and') {return baseresoneand(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='or') {return baseresoneor(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (findq(operator(p),prims))
     {return baseresoneprimitive(p,pl,al,cont,prims,pos,neg,facts,rules)};
  if (answer = baseresonebackground(p,pl,al,cont,prims,pos,neg,facts,rules))
     {return answer};  return baseresoners(p,pl,al,cont,prims,pos,neg,facts,rules)}function baseresoneatom (p,pl,al,cont,prims,pos,neg,facts,rules) {if (p==='true') {return baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules)};  if (p==='false') {return false};  if (findq(p,prims))
     {return baseresoneprimitive(p,pl,al,cont,prims,pos,neg,facts,rules)};
  if (answer = baseresonebackground(p,pl,al,cont,prims,pos,neg,facts,rules))
     {return answer};  return baseresoners(p,pl,al,cont,prims,pos,neg,facts,rules)}function baseresonesame (p,pl,al,cont,prims,pos,neg,facts,rules) {var ol = seq();
  var answer = false;  if (vnifyp(p[1],al,p[2],al,ol))     {if (answer = baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
         {backup(ol); return answer};
      backup(ol)}
  return false}function baseresonedistinct (p,pl,al,cont,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules)}function baseresonenot (p,pl,al,cont,prims,pos,neg,facts,rules) {var answers = seq();
  baseresall(p[1],seq(),al,nil,answers,prims,seq(),seq(),facts,rules);
  answers = flipsafely(answers);
  for (var i=0; i<answers.length; i++)
      {if (compatiblep(answers[i][0],answers[i][1],pos,neg))
          {var newpos = pos.concat(answers[i][0]);
           var newneg = neg.concat(answers[i][1]);
           var answer = false;
           if (answer = baseresoneexit(pl,al,cont,prims,newpos,newneg,facts,rules))
              {return answer}}};
  return false}

function baseresoneand (p,pl,al,cont,prims,pos,neg,facts,rules) {pl = concatenate(tail(p),pl)
  return baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules)}function baseresoneor (p,pl,al,cont,prims,pos,neg,facts,rules) {var answer = false;
  for (var i=0; i<p.length; i++)
      {if (answer = baseresone(p[i],pl,al,cont,prims,pos,neg,facts,rules))
          {return answer}};
  return false}function baseresoneprimitive (p,pl,al,cont,prims,pos,neg,facts,rules) {p = pluug(p,al);
  if (groundp(p) & !find(p,neg))
     {pos = newadjoin(p,pos);
      return baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules)};
  return false}function baseresonebackground (p,pl,al,cont,prims,pos,neg,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var ol = seq();       var answer = false
       if (vnifyp(data[i],al,p,al,ol))          {if (answer = baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
              {backup(ol); return answer};           backup(ol)}};
  return false}function baseresoners (p,pl,al,cont,prims,pos,neg,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               if (answer = baseresone(data[i][2],ql,bl,nc,prims,pos,neg,facts,rules))
                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = baseresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
                    {backup(ol); return answer};                 backup(ol)}}};
  return false}function baseresoneexit (pl,al,cont,prims,pos,neg,facts,rules) {if (pl.length!==0) {return baseresone(pl[0],tail(pl),al,cont,prims,pos,neg,facts,rules)};  if (nullp(cont)) {return seq(pos,neg)};  return baseresoneexit (car(cont)[0],car(cont)[1],car(cont)[2],prims,pos,neg,facts,rules)}

//------------------------------------------------------------------------------function baseresall (p,pl,al,cont,results,prims,pos,neg,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return baseresallatom(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='same') {return baseresallsame(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='distinct') {return baseresalldistinct(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='not') {return baseresallnot(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='and') {return baseresalland(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='or') {return baseresallor(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (findq(operator(p),prims))
     {return baseresallprimitive(p,pl,al,cont,results,prims,pos,neg,facts,rules)};
  baseresallbackground(p,pl,al,cont,results,prims,pos,neg,facts,rules);  return baseresallrs(p,pl,al,cont,results,prims,pos,neg,facts,rules)}function baseresallatom (p,pl,al,cont,results,prims,pos,neg,facts,rules) {if (p==='true') {return baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)};  if (p==='false') {return false};  if (findq(p,prims)) {return baseresallprimitive(p,pl,al,cont,results,prims,pos,neg,facts,rules)};
  baseresallbackground(p,pl,al,cont,results,prims,pos,neg,facts,rules);  return baseresallrs(p,pl,al,cont,results,prims,pos,neg,facts,rules)}function baseresallsame (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
      backup(ol)}}function baseresalldistinct (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)}function baseresallnot (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var answers = seq();
  baseresall(p[1],seq(),al,nil,answers,prims,seq(),seq(),facts,rules);
  answers = flipsafely(answers);
  for (var i=0; i<answers.length; i++)
      {if (compatiblep(answers[i][0],answers[i][1],pos,neg))
          {newpos = pos.concat(answers[i][0]);
           newneg = neg.concat(answers[i][1]);
           baseresallexit(pl,al,cont,results,prims,newpos,newneg,facts,rules)}};
  return false}

function baseresalland (p,pl,al,cont,results,prims,pos,neg,facts,rules) {baseresallexit(concatenate(tail(p),pl),al,cont,results,prims,pos,neg,facts,rules)}function baseresallor (p,pl,al,cont,results,prims,pos,neg,facts,rules) {for (var i=0; i<p.length; i++)
      {baseresall(p[i],pl,al,cont,results,prims,pos,neg,facts,rules)}}function baseresallprimitive (p,pl,al,cont,results,prims,pos,neg,facts,rules) {p = pluug(p,al);
  if (groundp(p) & !find(p,neg))
     {pos = newadjoin(p,pos);
      return baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)};
  return false}function baseresallbackground (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var ol = seq();       if (vnifyp(data[i],al,p,al,ol))          {baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);           backup(ol)}}}function baseresallrs (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               baseresall(data[i][2],ql,bl,nc,results,prims,pos,neg,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {baseresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);                 backup(ol)}}}}function baseresallexit (pl,bl,cont,results,prims,pos,neg,facts,rules) {if (pl.length!==0) {return baseresall(pl[0],tail(pl),bl,cont,results,prims,pos,neg,facts,rules)};  if (nullp(cont)) {results.push(seq(pos,neg)); return true};  return baseresallexit (car(cont)[0],car(cont)[1],car(cont)[2],results,prims,pos,neg,facts,rules)}

//------------------------------------------------------------------------------

function flipsafely (answers)
 {return flip(seq(),seq(),0,answers,seq())}

function flip (pos,neg,n,inputs,outputs)
 {if (n>=inputs.length)
     {outputs[outputs.length] = seq(neg,pos);
      return outputs};
  for (var i=0; i<inputs[n][0].length; i++)
      {flip(pos.concat(seq(inputs[n][0][i])),neg,n+1,inputs,outputs)}
  for (var i=0; i<inputs[n][1].length; i++)
      {flip(pos,neg.concat(seq(inputs[n][1][i])),n+1,inputs,outputs)}
  return outputs}function compatiblep (newpos,newneg,pos,neg)
 {for (var i=0; i<newpos.length; i++)
      {if (find(newpos[i],neg)) {return false}};
  for (var i=0; i<newneg.length; i++)
      {if (find(newneg[i],pos)) {return false}};
  for (var i=0; i<pos.length; i++)
      {if (find(pos[i],newneg)) {return false}};
  for (var i=0; i<neg.length; i++)
      {if (find(neg[i],newpos)) {return false}};
  return true}

//------------------------------------------------------------------------------
// compresidue// compresidues//------------------------------------------------------------------------------function compresidue (query,prims,facts,rules) {var answer = compresone(query,seq(),seq(),nil,prims,seq(),seq(),facts,rules);
  if (answer) {return maksand(answer[0])};
  return false}

function compresidues (query,prims,facts,rules) {var answers = seq();  compresall(query,seq(),seq(),nil,answers,prims,seq(),seq(),facts,rules);
  for (var i=0; i<answers.length; i++) {answers[i] = maksand(answers[i][0])};
  return uniquify(answers)}

//------------------------------------------------------------------------------function compresone (p,pl,al,cont,prims,pos,neg,facts,rules) {inferences = inferences + 1;  var answer = false;
  if (symbolp(p)) {return compresoneatom(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='same') {return compresonesame(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='distinct') {return compresonedistinct(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='matches') {return compresonematches(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (builtinp(p[0])) {return compresonecall(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (mathp(p[0])) {return compresonemath(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (listop(p[0])) {return compresonelist(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='map') {return compresonemap(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='provable') {return compresone(p[1],pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='not') {return compresonenot(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='and') {return compresoneand(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (p[0]==='or') {return compresoneor(p,pl,al,cont,prims,pos,neg,facts,rules)}  if (findq(operator(p),prims))
     {return compresoneprimitive(p,pl,al,cont,prims,pos,neg,facts,rules)};
  if (answer = compresonebackground(p,pl,al,cont,prims,pos,neg,facts,rules))
     {return answer};  return compresoners(p,pl,al,cont,prims,pos,neg,facts,rules)}function compresoneatom (p,pl,al,cont,prims,pos,neg,facts,rules) {if (p==='true') {return compresoneexit(pl,al,cont,prims,pos,neg,facts,rules)};  if (p==='false') {return false};  if (findq(p,prims))
     {return compresoneprimitive(p,pl,al,cont,prims,pos,neg,facts,rules)};
  if (answer = compresonebackground(p,pl,al,cont,prims,pos,neg,facts,rules))
     {return answer};  return compresoners(p,pl,al,cont,prims,pos,neg,facts,rules)}function compresonesame (p,pl,al,cont,prims,pos,neg,facts,rules) {var ol = seq();
  var answer = false;  if (vnifyp(p[1],al,p[2],al,ol))     {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
         {backup(ol); return answer};
      backup(ol)}
  return false}function compresonedistinct (p,pl,al,cont,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return compresoneexit(pl,al,cont,prims,pos,neg,facts,rules)}function compresonematches (p,pl,al,cont,prims,pos,neg,facts,rules) {var str = pluug(p[1],al,al)
  if (symbolp(str))     {var matches = str.match(p[2]);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var ol = seq();               if (vnifyp(p[3],al,matches[i],al,ol))                  {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))                      {backup(ol); return answer};
                   backup(ol)}}}};  return false}function compresonecall (p,pl,al,cont,prims,pos,neg,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  var ol = seq();
  var answer = false;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}
function compresonemath (p,pl,al,cont,prims,pos,neg,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();
  var answer = false;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}

function compresonelist (p,pl,al,cont,prims,pos,neg,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();
  var answer = false;  if (vnifyp(p[p.length-1],al,val,al,ol))
     {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
         {backup(ol); return answer};
      backup(ol)};
  return false}
function compresonemap (p,pl,al,cont,prims,pos,neg,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {var answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules);
      backup(ol);
      return answer};
  return false}

function compresonenot (p,pl,al,cont,prims,pos,neg,facts,rules) {var answers = seq();
  compresall(p[1],seq(),al,nil,answers,prims,seq(),seq(),facts,rules);
  answers = flipsafely(answers);
  for (var i=0; i<answers.length; i++)
      {if (compatiblep(answers[i][0],answers[i][1],pos,neg))
          {var newpos = pos.concat(answers[i][0]);
           var newneg = neg.concat(answers[i][1]);
           var answer = false;
           if (answer = compresoneexit(pl,al,cont,prims,newpos,newneg,facts,rules))
              {return answer}}};
  return false}

function compresoneand (p,pl,al,cont,prims,pos,neg,facts,rules) {pl = concatenate(tail(p),pl)
  return compresoneexit(pl,al,cont,prims,pos,neg,facts,rules)}function compresoneor (p,pl,al,cont,prims,pos,neg,facts,rules) {var answer = false;
  for (var i=0; i<p.length; i++)
      {if (answer = compresone(p[i],pl,al,cont,prims,pos,neg,facts,rules))
          {return answer}};
  return false}function compresoneprimitive (p,pl,al,cont,prims,pos,neg,facts,rules) {p = pluug(p,al);
  if (groundp(p) & !find(p,neg))
     {pos = newadjoin(p,pos);
      return compresoneexit(pl,al,cont,prims,pos,neg,facts,rules)};
  return false}function compresonebackground (p,pl,al,cont,prims,pos,neg,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var ol = seq();       var answer = false
       if (vnifyp(data[i],al,p,al,ol))          {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
              {backup(ol); return answer};           backup(ol)}};
  return false}function compresoners (p,pl,al,cont,prims,pos,neg,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();
       var answer = false;       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               if (answer = compresone(data[i][2],ql,bl,nc,prims,pos,neg,facts,rules))
                  {backup(ol); return answer};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (answer = compresoneexit(pl,al,cont,prims,pos,neg,facts,rules))
                    {backup(ol); return answer};                 backup(ol)}}};
  return false}function compresoneexit (pl,al,cont,prims,pos,neg,facts,rules) {if (pl.length!==0) {return compresone(pl[0],tail(pl),al,cont,prims,pos,neg,facts,rules)};  if (nullp(cont)) {return seq(pos,neg)};  return compresoneexit (car(cont)[0],car(cont)[1],car(cont)[2],prims,pos,neg,facts,rules)}

//------------------------------------------------------------------------------function compresall (p,pl,al,cont,results,prims,pos,neg,facts,rules) {inferences = inferences + 1;  if (symbolp(p)) {return compresallatom(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='same') {return compresallsame(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='distinct') {return compresalldistinct(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='matches') {return compresallmatches(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (builtinp(p[0])) {return compresallcall(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (mathp(p[0])) {return compresallmath(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (listop(p[0])) {return compresalllist(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='map') {return compresallmap(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='provable') {return compresall(p[1],pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='not') {return compresallnot(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='and') {return compresalland(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (p[0]==='or') {return compresallor(p,pl,al,cont,results,prims,pos,neg,facts,rules)}  if (findq(operator(p),prims))
     {return compresallprimitive(p,pl,al,cont,results,prims,pos,neg,facts,rules)};
  compresallbackground(p,pl,al,cont,results,prims,pos,neg,facts,rules);  return compresallrs(p,pl,al,cont,results,prims,pos,neg,facts,rules)}function compresallatom (p,pl,al,cont,results,prims,pos,neg,facts,rules) {if (p==='true') {return compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)};  if (p==='false') {return false};  if (findq(p,prims)) {return compresallprimitive(p,pl,al,cont,results,prims,pos,neg,facts,rules)};
  compresallbackground(p,pl,al,cont,results,prims,pos,neg,facts,rules);  return compresallrs(p,pl,al,cont,results,prims,pos,neg,facts,rules)}function compresallsame (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
      backup(ol)}}function compresalldistinct (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)}function compresallmatches (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var str = pluug(p[1],al,al)
  if (symbolp(str))     {var matches = str.match(p[2]);
      if (matches!=null)         {for (var i=0; i<matches.length; i++)              {var ol = seq();               if (vnifyp(p[3],al,matches[i],al,ol))                  {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
                   backup(ol)}}}};  return false}function compresallcall (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (varp(arg)) {return false} else {args[args.length] = arg}};
  var val = eval(p[0]).apply(null,args);
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
      backup(ol)};
  return false}
function compresallmath (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var args = seq();
  for (var i=1; i<p.length-1; i++)
      {var arg = numberize(pluug(p[i],al,al));
       if (isNaN(arg)) {return false};
       args[args.length] = arg};
  var val = stringize(Math[p[0]].apply(null,args));
  var ol = seq();  if (vnifyp(p[p.length-1],al,val,al,ol))
     {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
      backup(ol)};
  return false}

function compresalllist (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var c = pluug(p[1],al,al);
  var s = numlistify(c);
  if (s===false) {return false};
  var val = stringize(eval(p[0]).call(null,s));
  var ol = seq();  if (vnifyp(p[2],al,val,al,ol))
     {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);
      backup(ol)};
  return false}
function compresallmap (x,p,pl,al,cont,results,prims,pos,neg,facts,rules) {if (!symbolp(p[1]) || varp(p[1])) {return false};
  var val = map(p[1],pluug(p[2],al,al),facts,rules);
  if (val===false) {return false};
  var ol = seq();  if (vnifyp(p[3],al,val,al,ol))
     {compresallexit(x,pl,al,cont,results,prims,pos,neg,facts,rules); backup(ol)};
  return false}

function compresallnot (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var answers = seq();
  compresall(p[1],seq(),al,nil,answers,prims,seq(),seq(),facts,rules);
  answers = flipsafely(answers);
  for (var i=0; i<answers.length; i++)
      {if (compatiblep(answers[i][0],answers[i][1],pos,neg))
          {newpos = pos.concat(answers[i][0]);
           newneg = neg.concat(answers[i][1]);
           compresallexit(pl,al,cont,results,prims,newpos,newneg,facts,rules)}};
  return false}

function compresalland (p,pl,al,cont,results,prims,pos,neg,facts,rules) {compresallexit(concatenate(tail(p),pl),al,cont,results,prims,pos,neg,facts,rules)}function compresallor (p,pl,al,cont,results,prims,pos,neg,facts,rules) {for (var i=0; i<p.length; i++)
      {compresall(p[i],pl,al,cont,results,prims,pos,neg,facts,rules)}}function compresallprimitive (p,pl,al,cont,results,prims,pos,neg,facts,rules) {p = pluug(p,al);
  if (groundp(p) & !find(p,neg))
     {pos = newadjoin(p,pos);
      return compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules)};
  return false}function compresallbackground (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var data = envvndexps(p,al,facts);
  for (var i=0; i<data.length; i++)      {var ol = seq();       if (vnifyp(data[i],al,p,al,ol))          {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);           backup(ol)}}}function compresallrs (p,pl,al,cont,results,prims,pos,neg,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0]==='rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               compresall(data[i][2],ql,bl,nc,results,prims,pos,neg,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {compresallexit(pl,al,cont,results,prims,pos,neg,facts,rules);                 backup(ol)}}}}function compresallexit (pl,al,cont,results,prims,pos,neg,facts,rules) {if (pl.length!==0) {return compresall(pl[0],tail(pl),al,cont,results,prims,pos,neg,facts,rules)};  if (nullp(cont)) {results.push(seq(pos,neg)); return true};  return compresallexit (car(cont)[0],car(cont)[1],car(cont)[2],results,prims,pos,neg,facts,rules)}

//------------------------------------------------------------------------------// Input and Output//------------------------------------------------------------------------------function readdata (str) {return parsedata(scan(str))}function read (str) {return parse(scan(str))}function readitems (str) {return parseitems(scan(str))}//------------------------------------------------------------------------------var input = '';var output = '';var current = 0;function scan (str) {input = str;  output = new Array(0);  var cur = 0;  var len = input.length;  while (cur < len)   {var charcode = input.charCodeAt(cur);    if (charcode===32 || charcode===13) {cur++}    else if (charcode===33) {output[output.length] = '!'; cur++}    else if (charcode===34) {cur = scanstring(cur)}    else if (charcode===35) {output[output.length] = '#'; cur++}    else if (charcode===37) {cur = scancomment(cur)}    else if (charcode===38) {output[output.length] = '&'; cur++}    else if (charcode===40) {output[output.length] = 'lparen'; cur++}    else if (charcode===41) {output[output.length] = 'rparen'; cur++}    else if (charcode===44) {output[output.length] = 'comma'; cur++}    else if (charcode===58) {cur = scanrulesym(cur)}    else if (charcode===60) {cur = scanbacksym(cur)}    else if (charcode===61) {cur = scanthussym(cur)}    else if (charcode===62) {output[output.length] = '>'; cur++}    else if (charcode===91) {output[output.length] = '['; cur++}
    else if (charcode===93) {output[output.length] = ']'; cur++}
    else if (charcode===123) {output[output.length] = '{'; cur++}    else if (charcode===124) {output[output.length] = '|'; cur++}    else if (charcode===125) {output[output.length] = '}'; cur++}    else if (charcode===126) {output[output.length] = '~'; cur++}    else if (idcharp(charcode)) {cur = scansymbol(cur)}    else cur++};  return output}function scanrulesym (cur) {if (input.length > cur+1 && input.charCodeAt(cur+1)===45)     {output[output.length] = ':-'; return cur+2};  if (input.length > cur+1 && input.charCodeAt(cur+1)===61)     {output[output.length] = ':='; return cur+2}  else {output[output.length] = ':'; return cur+1}}function scanbacksym (cur) {if (input.length > cur+1 && input.charCodeAt(cur+1)===61)     {if (input.length > cur+2 && input.charCodeAt(cur+2)===62)         {output[output.length] = '<=>'; return cur+3}      else {output[output.length] = '<='; return cur+2}}  else {output[output.length] = '<'; return cur+1}}function scanthussym (cur) {if (input.length>cur+1 && input.charCodeAt(cur+1)===62)     {output[output.length] = '=>'; return cur+2};  if (input.length>cur+2 && input.charCodeAt(cur+1)===61
                         && input.charCodeAt(cur+2)===62)     {output[output.length] = '==>'; return cur+3};  output[output.length] = '=';
  return cur+1}function scansymbol (cur) {var n = input.length;  var exp = '';  while (cur < n)   {if (idcharp(input.charCodeAt(cur))) {exp = exp + input.charAt(cur); cur++}    else break};  if (exp!=='') {output[output.length] = exp};  return cur}function scanstring (cur) {var exp = '"';  cur++;  while (cur < input.length)   {exp = exp + input.charAt(cur);    if (input.charCodeAt(cur)===34) {cur++; break};    cur++};  output[output.length] = exp;  return cur}

function scancomment (cur) {while (cur<input.length &&
         input.charCodeAt(cur)!==10 && input.charCodeAt(cur)!==13)
   {cur++};  return cur}function letterp (charcode) {return ((charcode >= 65 && charcode <= 90) ||          (charcode >= 97 && charcode <= 122))}function idcharp (charcode) {if (charcode===42) {return true};  if (charcode===43) {return true};  if (charcode===45) {return true};  if (charcode===46) {return true};  if (charcode===47) {return true};  if (charcode >= 48 && charcode <= 57) {return true};  if (charcode >= 65 && charcode <= 90) {return true};  if (charcode >= 97 && charcode <= 122) {return true};  if (charcode===95) {return true};  return false}//------------------------------------------------------------------------------function parsedata (str) {str.push('eof');  input = str;  current = 0;  exp = new Array(0);  while (current < input.length && input[current]!=='eof')   {exp[exp.length] = parsexp('lparen','rparen')};  return exp}function parseitems (str) {str.push('eof');  input = str;  current = 0;  exp = seq();  while (current < input.length && input[current]!=='eof')   {if (input[current]==='comma') {current++};    exp[exp.length] = parsexp('lparen','rparen')};  return exp}function parse (str) {str.push('eof');  input = str;  current = 0;  return parsexp('lparen','rparen')}function parsexp (lop,rop) {var left = parseprefix(rop);  while (current < input.length)   {if (input[current]==='eof') {return left}    else if (input[current]==='lparen') {left = parseatom(left)}    else if (input[current]==='.') {current++; return(left)}    else if (precedencep(lop,input[current])) {return left}    else {left = parseinfix(left,input[current],rop)}};  return left}function parseprefix (rop) {var left = input[current];  current++;  if (left==='lparen') {left = parsexp('lparen','rparen'); current++; return left};  if (left==='#') {return makeprovable(parsexp('#',rop))};  if (left==='~') {return makenegation(parsexp('~',rop))};  if (left==='{') {return parseclause()};
  if (left==='[') {return parselist()};  return left}function parseatom (left) {var exp = parseparenlist();  exp.unshift(left);  return exp}function parseparenlist () {var exp = new Array(0);  current++;  if (input[current]==='rparen') {current++; return exp};  while (current < input.length)   {exp.push(parsexp('comma','rparen'));    if (input[current]==='rparen') {current++; return exp};    if (input[current]==='comma') {current++} else {return exp}};  return exp}function parseclause () {var exp = seq('clause');  if (input[current]==='}') {current++; return exp};
  while (current < input.length)   {exp.push(parsexp('comma','rparen'));    if (input[current]==='}') {current++; return exp};    if (input[current]==='comma') {current++}    else {return exp}};  return exp}

function parselist ()
 {if (input[current]===']') {current++; return nil};
  var head = parsexp('comma','comma');
  return seq('cons',head,parselistexp())}

function parselistexp ()
 {if (input[current]===']') {current++; return nil};
  if (input[current]==='comma')  
     {current++; return seq('cons',parsexp('comma','comma'),parselistexp())};
  return nil}

function parseinfix (left,op,rop) {if (op==='!') {return parsecons(left,rop)};  if (op===':') {return parsequantifier(left,rop)};  if (op==='&') {return parseand(left,rop)};  if (op==='|') {return parseor(left,rop)};  if (op==='<=>') {return parseequivalence(left,rop)};  if (op==='=>') {return parseimplication(left,rop)};  if (op==='<=') {return parsereduction(left,rop)};  if (op===':-') {return parserule(left,rop)};  if (op==='==>') {return parsetransition(left,rop)};  return left}

function parsecons (left,rop) {current++;  var dum = parsexp('!',rop);  return seq('cons',left,dum)}function parsequantifier (left,rop) {current++;  if (left[0]==='A') {return makeuniversal(left.slice(1,left.length),parsexp(':',rop))};  if (left[0]==='E') {return makeexistential(left.slice(1,left.length),parsexp(':',rop))};  return makeuniversal(left,parsexp(':',rop))}function parseand (left,rop) {current++;  return makeconjunction(left,parsexp('&',rop))}function parseor (left,rop) {current++;  return makedisjunction(left,parsexp('|',rop))}function parseequivalence (left,rop) {current++;  var dum = parsexp('<=>',rop);  return makeequivalence(left,dum)}function parseimplication (left,rop) {current++;  var dum = parsexp('=>',rop);  return makeimplication(left,dum)}function parsereduction (left,rop) {current++;  var dum = parsexp('<=',rop);  return makereduction(left,dum)}function parserule (left,rop) {current++;  var dum = parsexp(':-',rop);  return makerule(left,dum)}function parsetransition (left,rop) {current++;  var dum = parsexp('==>',rop);  return maketransition(left,dum)}function precedencep (lop,rop) {var dum = pp(lop,rop);  return dum}function pp (lop,rop) {if (lop==='!') {return rop!=='!'};  if (lop==='=') {return rop!=='!' && rop!=='='};  if (lop===':') {return rop!=='!' && rop!=='=' && rop!==':'};  if (lop==='#') {return rop!=='!' && rop!=='=' && rop!==':'};  if (lop==='~') {return rop!=='!' && rop!=='=' && rop!==':'};  if (lop==='&') {return rop!=='!' && rop!=='=' && rop!==':'};  if (lop==='|') {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&'};  if (lop==='=>')
     {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&
             rop!=='=>' && rop!=='<=' && rop!=='<=>'};  if (lop==='<=')
     {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&
             rop!=='=>' && rop!=='<=' && rop!=='<=>'};  if (lop==='<=>')
     {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&
             rop!=='=>' && rop!=='<=' && rop!=='<=>'};  if (lop===':-')
     {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&
             rop!=='=>' && rop!=='<=' && rop!=='<=>'};
  if (lop==='==>')
     {return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&
             rop!=='=>' && rop!=='<=' && rop!=='<=>'};  return rop!=='!' && rop!=='=' && rop!==':' && rop!=='&' && rop!=='|' &&         rop!=='=>' && rop!=='<=' && rop!=='<=>' && rop!==':-' && rop!=='==>'}function parenp (lop,op,rop) {return precedencep(lop,op) || !precedencep(op,rop)}//------------------------------------------------------------------------------// readkifdata// readkif//------------------------------------------------------------------------------function readkifdata (str) {return kifdata(kifscan(str))}function readkif (str) {return kif(kifscan(str))}function kifscan (str) {input = str;  output = new Array(0);  var cur = 0;  var len = input.length;  while (cur < len)   {var charcode = input.charCodeAt(cur);    if (charcode===32 || charcode===13) {cur++}    else if (charcode===34) {cur = scanstring(cur)}    else if (charcode===40) {output[output.length] = 'lparen'; cur++}    else if (charcode===41) {output[output.length] = 'rparen'; cur++}    else if (charcode===59) {cur = kifscancomment(cur)}    else if (charcode===63) {cur = kifscanvariable(cur)}    else if (kifidcharp(charcode)) {cur = kifscansymbol(cur)}    else cur++};  return output}function kifscansymbol (cur) {var exp = '';  while (cur < input.length)   {if (kifidcharp(input.charCodeAt(cur))) {exp = exp + input[cur]; cur++}    else break};  if (exp!=='') {output[output.length] = exp};  return cur}function kifscanvariable (cur) {cur++;  var exp = '';  if (letterp(input.charCodeAt(cur)))     {exp=input.slice(cur,cur+1).toUpperCase(); cur++}     else {exp = 'VV'};  while (cur < input.length)   {if (kifidcharp(input.charCodeAt(cur))) {exp = exp + input[cur]; cur++}    else break};  if (exp!=='') {output[output.length] = exp};  return cur}function kifscanstring (cur) {var exp = '';  cur++  while (cur < input.length && input.charCodeAt(cur)!==34)        {exp = exp + input[cur]; cur++};  cur++;  output[output.length] = exp  return cur}function kifscancomment (cur) {while (cur < input.length && input.charCodeAt(cur)!==10 && input.charCodeAt(cur)!==13) {cur++};  return cur}function kifidcharp (charcode) {if (charcode===45) {return true};  if (charcode===46) {return true};  if (charcode===60) {return true};  if (charcode===61) {return true};  if (charcode >= 48 && charcode <= 57) {return true};  if (charcode >= 65 && charcode <= 90) {return true};  if (charcode >= 97 && charcode <= 122) {return true};  if (charcode===95) {return true};  return false}function kifdata (str) {str.push('eof');  input = str;  current = 0;  exp = new Array(0);  while (current < input.length && input[current]!=='eof')   {exp[exp.length] = kifexp()};  return exp}function kif (str) {str.push('eof');  input = str;  current = 0;  return kifexp()}function kifexp () {var lexeme = input[current];  if (lexeme==='eof') {return nil};  if (lexeme==='<=') {current++; return 'rule'};  if (lexeme==='lparen') {return kifparenlist()};  current++; return lexeme}function kifparenlist () {var exp = new Array(0);  current++;  if (input[current]==='rparen') {current++; return exp};  while (current < input.length)   {exp.push(kifexp());    if (input[current]==='rparen') {current++; return exp}};  return exp}//------------------------------------------------------------------------------

function printseq (p) {if (p===true) {return 'true'};
  if (p===false) {return 'false'};
  if (typeof p==='number') {return p};  if (typeof p==='string') {return '"' + p + '"'};  var n = p.length;  var exp = '(';  if (n>0) {exp += printseq(p[0])};  for (var i=1; i<n; i++)      {exp = exp + ' ' + printseq(p[i])}  exp += ')';  return exp}

function printspaces (n)
 {var exp = '';
  for (var i=0; i<n; i++) {exp += '  '};
  return exp}

//------------------------------------------------------------------------------
function printdata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + printit(data[i]) + '<br/>'}  return exp}function printem (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + printit(data[i]) + '\r'}  return exp}function printit (p) {if (p==='rule') {return '<='};
  if (p===null) {return ''};  if (varp(p)) {return '?' + p};  if (symbolp(p)) {return p};  var n = p.length;  var exp = '(';  if (n>0) {exp += printit(p[0])};  for (var i=1; i<n; i++)      {exp = exp + ' ' + printit(p[i])}  exp += ')';  return exp}//------------------------------------------------------------------------------function doxml () {var win = window.open();  //win.document.open('text/html');  win.document.writeln('&lt;?xml version="1.0"?&gt;<br/>\n');  win.document.writeln('&lt;?xml-stylesheet type="text/xsl" href="../stylesheets/proof.xsl"?&gt;<br/>\n');  win.document.write(xmlproof());  win.document.close()}function xmlproof () {var exp = '';  exp += '&lt;proof&gt;<br/>\n';  for (var i=1; i<proof.length; i++)      {exp += '  &lt;step&gt;<br/>';       exp += '    &lt;number&gt;' + i + '&lt;/number&gt;<br/>\n';       exp += '    &lt;sentence&gt;' + grind(proof[i][1]) + '&lt;/sentence&gt;<br/>\n';       exp += '    &lt;justification&gt;' + prettify(proof[i][2]) + '&lt;/justification&gt;<br/>\n';       for (var j=3; j<proof[i].length; j++)           {exp += '    &lt;antecedent&gt;' + proof[i][j] + '&lt;/antecedent&gt;<br/>\n'};       exp += '  &lt;/step&gt;<br/>\n'};  exp += '&lt;/proof&gt;<br/>\n';  return exp}function xmlify (str) {str = str.replace('&','&amp;');  str = str.replace('<=>','&lt;=&gt;');  return str}//------------------------------------------------------------------------------function smoothdata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + smooth(data[i]) + '<br/>'}  return exp}function smooth (p) {if (symbolp(p)) {return p};  var exp = p[0] + '(';  if (p.length > 1) {exp += smooth(p[1])};  for (var i=2; i<p.length; i++)      {exp += ',' + smooth(p[i])}  exp += ')';  return exp}//------------------------------------------------------------------------------function grindproof (proof) {var exp = '';  exp = exp + '<table cellpadding="4" cellspacing="0" border="1">';  exp = exp + '<tr bgcolor="#bbbbbb">';  exp = exp + '<td>&nbsp;</td>'; //exp = exp + '<td><input type="checkbox" name="Selection"/></td>';  exp = exp + '<th>Step</th><th>Proof</th><th>Justification</th>';  exp = exp + '</tr>';  for (var i=0; i<proof.length; i=i+3)      {exp = exp + '<tr id="0">';       exp = exp + '<td bgcolor="#eeeeee"><input id="' + (i/3 + 1) + '" type="checkbox"/></td>';       exp = exp + '<td align="center" bgcolor="#eeeeee">' + (i/3 + 1) + '</td>';       exp = exp + '<td>' + grind(proof[i+1]) + '</td>';       exp = exp + '<td bgcolor="#eeeeee">' + proof[i+2] + '</td>';       exp = exp + '</tr>'};         exp = exp + '</table>';  return exp}//------------------------------------------------------------------------------function grinddata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + grind(data[i]) + '<br/>'}  return exp}function grindem (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + grind(data[i]) + '\r'}  return exp}function grind (p) {return grindit(p,'lparen','rparen')}function grindit (p,lop,rop) {if (p==='nil') {return '[]'};
  if (symbolp(p)) {return p};  if (p[0]==='cons') {return grindcons(p,lop,rop)}
  if (p[0]==='definition') {return grinddefinition(p,lop,rop)};  if (p[0]==='provable') {return grindprovable(p,rop)};  if (p[0]==='not') {return grindnegation(p,rop)};  if (p[0]==='and') {return grindand(p,lop,rop)};  if (p[0]==='or') {return grindor(p,lop,rop)};  if (p[0]==='equivalence') {return grindequivalence(p,lop,rop)};  if (p[0]==='implication') {return grindimplication(p,lop,rop)};  if (p[0]==='reduction') {return grindreduction(p,lop,rop)};  if (p[0]==='rule') {return grindrule(p,lop,rop)};  if (p[0]==='transition') {return grindtransition(p,lop,rop)};  if (p[0]==='clause') {return grindclause(p)};  if (p[0]==='forall') {return grinduniversal(p,lop,rop)};  if (p[0]==='exists') {return grindexistential(p,lop,rop)};  return grindatom(p)}

function grindcons (p,lop,rop)
 {if (listp(p)) {return grindlist(p)};
  var exp = '';  var parens = parenp(lop,'!',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'!') + '!' + grindit(p[2],'!',rop);  if (parens) {exp = exp + ')'};  return exp}

function grindlist (p)
 {var out = '[' + grind(p[1]);
  p = p[2];
  while (!symbolp(p) && p[0]==='cons')
   {out = out + ',' + grind(p[1]); p = p[2]};
  if (p!=='nil') {out = out + '|' + grind(p)};
  out = out + ']';
  return out}function grindatom (p) {var n = p.length;  var exp = p[0] + '(';  if (n>1) {exp += grind(p[1])};  for (var i=2; i<n; i++)      {exp = exp + ',' + grind(p[i])}  exp += ')';  return exp}function grinddefinition (p,lop,rop) {var exp = '';  var parens = parenp(lop,':=',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,':=') + ' := ' + grindit(p[2],':=',rop);  if (parens) {exp = exp + ')'};  return exp}function grindprovable (p,rop) {return '#' + grindit(p[1],'#',rop)}function grindnegation (p,rop) {return '~' + grindit(p[1],'~',rop)}function grindand (p,lop,rop) {if (p.length===1) {return 'true'};  if (p.length===2) {return grind(p[1],lop,rop)};  var exp;  exp = grindleft(lop,'&',rop) + grindit(p[1],lop,'&');  for (var i=2; i<p.length-1; i++)      {exp = exp + ' & ' + grindit(p[i],'&','&')};  exp = exp + ' & ' + grindit(p[p.length-1],'&',rop) + grindright(lop,'&',rop);  return exp}function grindor (p,lop,rop) {var exp;  if (p.length===1) {return 'false'};  if (p.length===2) {return grind(p[1],lop,rop)};  exp = grindleft(lop,'|',rop) + grindit(p[1],lop,'|');  for (var i=2; i<p.length-1; i++)      {exp = exp + ' | ' + grindit(p[i],'|','|')};  exp = exp + ' | ' + grindit(p[p.length-1],'|',rop) + grindright(lop,'|',rop);  return exp}function grindequivalence (p,lop,rop) {var exp = '';  var parens = parenp(lop,'<=>',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'<=>') + ' <=> ' + grindit(p[2],'<=>',rop);  if (parens) {exp = exp + ')'};  return exp}function grindimplication (p,lop,rop) {var exp = '';  var parens = parenp(lop,'=>',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'&')  for (var i=2; i<p.length-1; i++)      {exp = exp + ' & ' + grindit(p[i],'&','&')};  exp = exp + ' => ' + grindit(p[p.length-1],'=>',rop);  if (parens) {exp = exp + ')'};  return exp}function grindreduction (p,lop,rop) {var exp = '';  var parens = parenp(lop,'<=',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'<=') + ' <= ' + grindit(p[2],'<=',rop);  if (parens) {exp = exp + ')'};  return exp}function grindrule (p,lop,rop) {var exp = grind(p[1]) + ' :- ';  if (p.length===2) {exp += 'true'}  else if (p.length===3) {exp += grindit(p[2],':-',rop)}  else {exp += grindit(p[2],lop,'&');        for (var i=3; i<p.length-1; i++)            {exp = exp + ' & ' + grindit(p[i],'&','&')};        exp += ' & ' + grindit(p[p.length-1],'&',rop)};  return exp}function grindtransition (p,lop,rop) {var exp = '';  var parens = parenp(lop,'==>',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'==>') + ' ==> ' + grindit(p[2],'==>',rop);  if (parens) {exp = exp + ')'};  return exp}

function grindclause (p) {var exp = '{';  if (p.length > 1) {exp = exp + grind(p[1])};  for (var i=2; i<p.length; i++)      {exp = exp + ',' + grind(p[i])};  exp = exp + '}';  return exp}function grinduniversal (p,lop,rop) {return grindleft(lop,':',rop) + 'A' + grindit(p[1],lop,':') + ':' + grindit(p[2],':',rop) + grindright(lop,':',rop)}function grindexistential (p,lop,rop) {return grindleft(lop,':',rop) + 'E' + grindit(p[1],lop,':') + ':' + grindit(p[2],':',rop) + grindright(lop,':',rop)}function grindleft (lop,op,rop) {if (precedencep(lop,op) || precedencep(rop,op)) {return "("}  return ""}function grindright (lop,op,rop) {if (precedencep(lop,op) || precedencep(rop,op)) {return ")"}  return ""}function grindalist (al) {var exp = '';  if (al===false) {return 'false'};  for (var l=al; !nullp(l); l=cdr(l))      {exp = exp + car(car(l)) + ' = ' + grind(cdr(car(l))) + '<br/>'}  return exp}//------------------------------------------------------------------------------function displayrules (rules)
 {exp = '';
  for (var i=0; i<rules.length; i++)
      {exp += displayrule(rules[i]) + '\n'};
  return exp}function displayrule (p)
 {if (symbolp(p)) {return p};
  if (p[0]==='rule') {return disprule(p)};  if (p[0]==='transition') {return disptransition(p)};  return grindatom(p)}

function disprule (p) {if (p.length==2) {return grind(p[1]) + ' :- true\n'};  if (p.length==3) {return grind(p[1]) + ' :- ' + grind(p[2]) + '\n'};  var exp = grind(p[1]) + ' :-\n';  for (var i=2; i<p.length-1; i++)      {exp = exp + '  ' + grind(p[i]) + ' &\n'};  exp +=  '  ' + grind(p[p.length-1]) + '\n';  return exp}

function disptransition (p) {if (p.length<2) {return ''};
  if (symbolp(p[2]) || p[2][0]!=='and')
     {return grind(p[1]) + ' ==> ' + grind(p[2]) + '\n'};
  if (p[2].length<4)
     {return grind(p[1]) + ' ==>\n  ' + grind(p[2]) + '\n'};
  var exp = grind(p[1]) + ' ==>\n';  for (var i=1; i<p[2].length-1; i++)      {exp = exp + '  ' + grind(p[2][i]) + ' &\n'};  exp +=  '  ' + grind(p[2][p.length-1]) + '\n';  return exp}

//------------------------------------------------------------------------------function displayproof (proof) {var exp = '';  exp = exp + '<table cellpadding="4" cellspacing="0" border="1">';  exp = exp + '<tr bgcolor="#bbbbbb">';  exp = exp + '<td><input type="checkbox" onClick="doselectall()"/></td>';  exp = exp + '<th>Step</th><th>Proof</th><th>Justification</th>';  exp = exp + '</tr>';  for (var i=1; i<proof.length; i++)      {exp = exp + '<tr id="0">';       exp = exp + '<td bgcolor="#eeeeee"><input id="' + i +                   '" type="checkbox"/></td>';       exp = exp + '<td align="center" bgcolor="#eeeeee">' + i + '</td>';       exp = exp + '<td>' + grind(proof[i][1]) + '</td>';       exp += '<td bgcolor="#eeeeee">';       exp += prettify(proof[i][2]);       if (proof[i].length > 3)          {exp += ': ' + proof[i][3];           for (var j=4; j<proof[i].length; j++) {exp += ', ' + proof[i][j]}};       exp += '</td>';       exp = exp + '</tr>'};         exp = exp + '</table>';  return exp}function prettify (str) {return str.replace('_',' ')}//------------------------------------------------------------------------------
// morefacts
// morerules
// loadfacts
// loadrules
// dumpfacts
// dumprules
//------------------------------------------------------------------------------

function morefacts (filename,target)
 {var contents = fs.readFileSync(filename).toString();
  var data = readdata(contents);
  definemorefacts(target,data);
  return true}

function morerules (filename,target)
 {var contents = fs.readFileSync(filename).toString();
  var data = readdata(contents);
  definemorerules(target,data);
  return true}

function loadfacts (filename,target)
 {var contents = fs.readFileSync(filename).toString();
  var data = readdata(contents);
  emptytheory(target);
  definemorefacts(target,data);
  return true}

function loadrules (filename,target)
 {var contents = fs.readFileSync(filename).toString();
  var data = readdata(contents);
  emptytheory(target);
  definemorerules(target,data);
  return true}

function dumpfacts (source,filename)
 {fs.writeFileSync(filename,showfacts(source));
  return true}

function showfacts (source)
 {var bases = getbases(source);
  var output = '';
  for (var i=0; i<bases.length; i++)
      {var facts = sentences(bases[i],source);
       for (j=0; j<facts.length; j++)
           {output += grind(facts[j]) + '\n'};
       output += '\n'};
  return output}
function dumprules (source,filename)
 {fs.writeFileSync(filename,showrules(source));
  return true}

function showrules (source)
 {var views = getviews(source);
  var output = '';
  for (var i=0; i<views.length; i++)
      {var rules = sentences(views[i],source);
       for (j=0; j<rules.length; j++)
           {output += grind(rules[j]) + '\n'};
       output += '\n'};
  return output}
//------------------------------------------------------------------------------
// finderrors
//------------------------------------------------------------------------------

function finderrors (data)
 {var errors = findarityerrors(data);
  errors = errors.concat(findsafetyerrors(data));
  errors = errors.concat(findstratificationerrors(data));
  return errors}

//------------------------------------------------------------------------------

function findarityerrors (data)
 {arities = seq();
  for (var i=0; i<data.length; i++)
      {arities = getarities(data[i],arities)};
  var errors = seq();
  for (rel in arities)
      {if (arities[rel]==='mixed')
          {errors[errors.length] = 'Mixed arity: ' + grind(rel)}};
  return errors}

function getarities (p,arities)
 {if (symbolp(p)) {return addarity(p,0,arities)}
  if (findq(p[0],aggregates))
     {return getarities(p[2],arities)};
  if (p[0]==='not') {return getarities(p[1],arities)};
  if (p[0]==='and' || p[0]==='or' || p[0]==='rule')
     {for (var i=1; i<p.length; i++)
          {arities = getarities(p[i],arities)};
      return arities};
  return addarity(p[0],p.length-1,arities)}

function addarity (x,n,arities)
 {if (arities[x]==null) {arities[x] = n; return arities};
  if (arities[x]===n) {return arities};
  arities[x] = 'mixed';
  return arities}

//------------------------------------------------------------------------------

function findsafetyerrors (data)
 {var errors = seq();
  for (var i=0; i<data.length; i++)
      {if (!safep(data[i]))
          {errors[errors.length] = 'Unsafe rule: ' + grind(data[i])}};
  return errors}

function safep (exp) {if (symbolp(exp)) {return true};  if (exp[0]==='rule') {return saferulep(exp)};  if (exp[0]==='transition') {return safetransitionp(exp)};  return groundp(exp)}function groundp (exp) {if (varp(exp)) {return false};  if (symbolp(exp)) {return true};  for (var i=0; i<exp.length; i++)      {if (!groundp(exp[i])) {return false}};  return true}function saferulep (rule) {var vs = seq();
  for (var i=2; i<rule.length; i++)
      {vs = safegoalp(rule[i],vs)
       if (vs===false) {return false}};
  return safeheadp(rule[1],vs)}
function safetransitionp (transition) {var vs = seq();
  for (var i=1; i<transition.length-1; i++)
      {vs = safegoalp(transition[i],vs)
       if (vs===false) {return false}};
  return safeheadp(transition[2],vs)}
function safegoalp (exp,vs)
 {if (symbolp(exp)) {return vs};
  if (exp[0]==='distinct')
     {if (groundedp(exp,vs)) {return vs} else {return false}};
  if (findq(exp[0],builtins))
     {for (var i=1; i<exp.length-1; i++)
          {if (!groundedp(exp[i],vs)) {return false}};
      return varsexp(exp[exp.length-1],vs)};  if (find(exp[0],aggregates))
     {if (!groundedp(exp[1],vars(exp[2]))) {return false};
      if (!safegoalp(exp[2],seq())) {return false};
      //if (!safegoalp(exp[2],vs.concat(vars(exp[1])))) {return false};
      return varsexp(exp[3],vs)};  if (exp[0]==='not')
     {if (groundedp(exp[1],vs)) {return vs} else {return false}};
  if (exp[0]==='and')
     {for (var i=1; i<exp.length; i++)
          {vs = safegoalp(exp[i],vs)
           if (vs===false) {return false}}
      return vs};
  return varsexp(exp,vs)}
function safeheadp (exp,vs)
 {var hs = vars(exp);
  for (var i=0; i<hs.length; i++)      {if (!findq(hs[i],vs)) {return false}};  return true}
function groundedp (exp,vs) {if (varp(exp)) {return find(exp,vs)};  if (symbolp(exp)) {return true};  for (var i=0; i<exp.length; i++)      {if (!groundedp(exp[i],vs)) {return false}};  return true}function operator (p)
 {if (symbolp(p)) {return p};
  if (p[0]=='not' || p[0]=='rule') {return operator(p[1])};
  return p[0]}

function operands (p)
 {if (symbolp(p)) {return seq()};
  if (p[0]=='not' || p[0]=='rule') {return operands(p[1])};
  return p.slice(1)}

//------------------------------------------------------------------------------

function findstratificationerrors (data)
 {var strata = getstrata(data);
  var errors = seq();
  for (var i=0; i<data.length; i++)
      {if (!checkstratifiedrecursion(data[i],strata))
          {errors[errors.length] = 'Unstratified Recursion: ' + grind(data[i])}};
  for (var i=0; i<data.length; i++)
      {if (!checkstratifiednegation(data[i],strata))
          {errors[errors.length] = 'Unstratified Negation: ' + grind(data[i])}};
  return errors}

function checkstratifiednegation(datum, strata)
 {if (symbolp(datum)) {return true};
  if (datum[0]!=='rule') {return true};
  var stratum = strata[operator(datum[1])];
  for (var j=2; j<datum.length; j++)
      {if (symbolp(datum[j])) {continue};
       if (datum[j][0]==='not')
          {if (strata[operator(datum[j][1])]>=stratum) {return false};
           continue};
       if (aggregatep(datum[j][0]))
          {var rs = getrelations(datum[j],seq());
           for (var k=0; k<rs.length; k++)
               {if (strata[rs[k]]>=stratum) {return false}}}};
   return true}

function checkstratifiedrecursion (datum,strata)
 {if (symbolp(datum)) {return true};
  if (datum[0]!=='rule') {return true};
  var stratum = strata[operator(datum[1])];
  var hs = seq(); //vars(datum[1]);
  var vs = seq();
  for (var j = 2; j<datum.length; j++)
      {if (symbolp(datum[j]) || (datum[j][0]!=='not' && !aggregatep(datum[j])))
          {if (strata[operator(datum[j])]>=stratum)
              {hs = varsexp(datum[j],hs)}
           else {vs = varsexp(datum[j],vs)}}};
  for (var i=0; i<hs.length; i++)
      {if (!findq(hs[i],vs)) {return false}};
  return true}

var succ = {}, stack = [], vertex = {}, _index = 0, scc = [];

function getstrata(data)
 {var scc = getscc(data);
  var stratum = 0;
  var strata = seq();
  for (var i = scc.length-1; i>=0; i--)
      {for (var j=0; j<scc[i].length; j++)
           {strata[scc[i][j]] = stratum};
       stratum++};
  return strata}

function getscc(data)
 {scc = [], _index = 0, stack = [], vertex = {}, succ = {};
  var rs = getallrelations(data)
  for (var i=0; i<rs.length; i++)
      {succ[rs[i]] = [];
       vertex[rs[i]] = {}}
  for (var i=0; i<data.length; i++)
      {if (data[i][0] == "rule")
          {var headrel = operator(data[i][1]);
           var relsucc = getallrelations(data[i].slice(2));
           for (var j = 0; j < relsucc.length; j++)
               {succ[relsucc[j]] = adjoin(headrel, succ[relsucc[j]])}}}
  for (var i=0; i<rs.length; i++)
      {if (typeof vertex[rs[i]].index == 'undefined') {visit(rs[i])}};
  return scc}

function visit(v)
 {vertex[v].index = _index;
  vertex[v].low = _index;
  _index++;
  stack.push(v);
  vertex[v].onstack = true;
  for (var i=0; i<succ[v].length; i++)
      {var w = succ[v][i];
       if (updateop(w)) continue;       if (typeof vertex[w].index=='undefined')
          {visit(w);
           vertex[v].low = Math.min(vertex[v].low,vertex[w].low)}
       else if (vertex[w].onstack)
               {vertex[v].low = Math.min(vertex[v].low,vertex[w].low)}}	
  if (vertex[v].low==vertex[v].index)
     {var _scc = [], w = null;
      while (w != v)
       {w = stack.pop();
        vertex[w].onstack = false;
        _scc.push(w)}
      scc.push(_scc)}}

function getallrelations (data)
 {var rs = seq();
  for (var i=0; i<data.length; i++)
      {rs = getrelations(data[i],rs)};
  return rs}

function getrelations (datum,rs)
 {if (symbolp(datum)) {return adjoin(datum,rs)};
  if (datum[0]==='not' || updateop(datum[0]))
     {return getrelations(datum[1],rs)};
  if (datum[0]==='rule' || datum[0]==='and' || datum[0]==='or')
     {for (var j=1; j<datum.length; j++) {rs = getrelations(datum[j],rs)};
      return rs};
  if (builtinp(datum[0])) {return rs};
  if (mathp(datum[0])) {return rs};
  if (listop(datum[0])) {return rs};
  if (aggregatep(datum[0])) {return getrelations(datum[2],rs)};
  return adjoin(datum[0],rs)}

//------------------------------------------------------------------------------// End//------------------------------------------------------------------------------