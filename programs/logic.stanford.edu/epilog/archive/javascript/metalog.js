//------------------------------------------------------------------------------// Sentential Representation//------------------------------------------------------------------------------function symbolp (x) {return typeof x == 'string'}function varp (x) {return typeof x == 'string' && x.length != 0 && x[0] != x[0].toLowerCase()}function constantp (x) {return typeof x == 'string' && x.length != 0 && x[0] == x[0].toLowerCase()}var counter = 0function newvar () {counter++;  return 'V' + counter}function newsym () {counter++;  return 'c' + counter}function seq () {var exp=new Array(arguments.length);  for (var i=0; i<arguments.length; i++) {exp[i]=arguments[i]};  return exp}function head (p) {return p[0]}function tail (l) {return l.slice(1,l.length)}function makeequality (x,y) {return seq('same',x,y)}function makeinequality (x,y) {return seq('distinct',x,y)}function makenegation (p) {return seq('not',p)}function makeconjunction (p,q) {if (p[0] == 'and') {return p.concat(seq(q))};  return seq('and',p,q)}function makedisjunction (p,q) {if (p[0] == 'or') {return p.concat(seq(q))};  return seq('or',p,q)}

function makereduction (head,body) {return seq('reduction',head,body)}function makeimplication (head,body) {return seq('implication',head,body)}function makeequivalence (head,body) {return seq('equivalence',head,body)}function makerule (head,body) {if (body.length == 0) {return head};  if (body[0] == 'and') {return seq('rule',head).concat(tail(body))};  return seq('rule',head,body)}

function makeuniversal (variable,scope) {return seq('forall',variable,scope)}function makeexistential (variable,scope) {return seq('exists',variable,scope)}function makeconditional (p,x,y) {return seq('if',p,x,y)}function makeclause (p,q) {return seq('clause',p,q)}function makedefinition (head,body) {if (!symbolp(body) & body[0]=='and')     {return seq('definition',head).concat(tail(body))}  else {return seq('definition',head,body)}}function makestep (sentence,justification,p1,p2) {var exp = new Array(3);  exp[0] = 'step';  exp[1] = sentence;  exp[2] = justification;  if (p1) {exp[3] = p1};  if (p2) {exp[4] = p2};  return exp}function makeproof () {var exp = new Array(1);  exp[0] = 'proof';  return exp}function maksatom (r,s) {return seq(r).concat(s)}function maksand (s) {if (s.length == 0) {return 'true'};  if (s.length == 1) {return s[0]};  return seq('and').concat(s)}function maksor (s) {if (s.length == 0) {return 'false'};
  if (s.length == 1) {return s[0]};  return seq('or').concat(s)}function negate (p)
 {if (symbolp(p)) {return makenegation(p)};
  if (p[0] == 'not') {return p[1]};
  return makenegation(p)}function adjoin (x,s) {if (!findq(x,s)) {s.push(x)};
  return s}function concatenate (l1,l2) {return l1.concat(l2)}function findq (x,s) {for (var i=0; i<s.length; i++) {if (x == s[i]) {return true}};  return false}function find (x,s) {for (var i=0; i<s.length; i++) {if (equalp(x,s[i])) {return true}};  return false}function subset (s1,s2) {for (var i=0; i<s1.length; i++)      {if (!find(s1[i],s2)) {return false}};  return true}function difference (l1, l2) {var answer = seq();  for (var i=0; i<l1.length; i++)      {if (!find(l1[i],l2)) {answer[answer.length] = l1[i]}};  return answer}function subst (x,y,z) {if (z == y) {return x};  if (symbolp(z)) {return z};  var exp = new Array(z.length);  for (var i=0; i<z.length; i++)      {exp[i] = subst(x,y,z[i])};  return exp}function substitute (p,q,r) {if (symbolp(r)) {if (r == p) {return q} else {return r}};  var exp = seq();
  for (var i=0; i<r.length; i++)      {exp[exp.length] = substitute(p,q,r[i])};  if (equalp(exp,p)) {return q} else {return exp}}function substitutions (p,q,r) {if (symbolp(r)) {if (r == p) {return seq(r,q)} else {return seq(r)}};  return substitutionsexp(p,q,r,0)}function substitutionsexp (p,q,r,n) {if (n == r.length) {return seq(seq())};  var firsts = substitutions(p,q,r[n]);  var rests = substitutionsexp(p,q,r,n+1);  var results = seq();  for (var i=0; i<firsts.length; i++)      {for (var j=0; j<rests.length; j++)           {exp = seq(firsts[i]).concat(rests[j]);            results[results.length] = exp;            if (equalp(exp,p)) {results[results.length] = q}}}  return results}function vars (x) {return varsexp(x,seq())}function varsexp (x,vs) {if (varp(x)) {return adjoin(x,vs)};  if (symbolp(x)) {return vs};  for (var i=0; i<x.length; i++) {vs = varsexp(x[i],vs)};
  return vs}function constants (x) {return constantsexp(x,seq())}function constantsexp (x,vs) {if (varp(x)) {return vs};  if (symbolp(x)) {return adjoin(x,vs)};  for (var i=1; i<x.length; i++) {vs = constantsexp(x[i],vs)};  return vs}function equalp (p,q) {if (symbolp(p)) {if (symbolp(q)) {return p==q} else {return false}};  if (symbolp(q)) {return false};  if (p.length != q.length) {return false};  for (var i=0; i<p.length; i++) {if (!equalp(p[i],q[i])) {return false}};  return true}
//------------------------------------------------------------------------------// Linked Lists//------------------------------------------------------------------------------var nil = 'nil'function nullp (l) {return l == 'nil'}function cons (x,l) {var cell = new Array(2);  cell[0] = x;  cell[1] = l;  return cell}function car (l) {return l[0]}function cdr (l) {return l[1]}function list () {var exp=nil;  for (var i=arguments.length; i>0; i--)
      {exp=cons(arguments[i-1],exp)};  return exp}function len (l) {var n = 0;  for (var m=l; m!=nil; m = cdr(m)) {n = n+1};  return n}function memberp (x,l) {if (nullp(l)) {return false};  if (equalp(car(l),x)) {return true};  if (memberp(x,cdr(l))) {return true};  return false}function append (l1,l2) {if (nullp(l1)) {return l2}
     else {return cons(car(l1),append(cdr(l1),l2))}}function nreverse (l) {if (nullp(l)) {return nil}  else {return nreversexp(l,nil)}}function nreversexp (l,ptr) {if (cdr(l) == nil) {l[1] = ptr; return l};  var rev = nreversexp(cdr(l),l);  l[1] = ptr;  return rev}function acons (x,y,al) {return cons(cons(x,y),al)}function assoc (x,al) {if (nullp(al)) {return false};  if (x == car(car(al))) {return car(al)};
  return assoc(x,cdr(al))}//------------------------------------------------------------------------------// matcher//------------------------------------------------------------------------------function matcher (x,y) {return match(x,y,nil)}function match (x,y,bl) {if (x == y) {return bl};  if (varp(x)) {return matchvar(x,y,bl)};  if (symbolp(x)) {return false};  return matchexp(x,y,bl)}function matchvar (x,y,bl) {var dum = assoc(x,bl);  if (dum != false) {return match(cdr(dum),y,bl)};  if (x == matchval(y,bl)) {return bl};  return acons(x,y,bl)}function matchval (y,bl) {if (varp(y))     {var dum = assoc(y,bl);      if (dum != false) {return matchval(cdr(dum),bl)};      return y};  return y}function matchexp(x,y,bl) {if (symbolp(y)) {return false};  var m = x.length;  var n = y.length;    if (m != n) {return false};  for (var i=0; i<m; i++)      {bl = match(x[i],y[i],bl);       if (bl == false) {return false}};  return bl}//------------------------------------------------------------------------------// Unification
//------------------------------------------------------------------------------function unifier (x,y) {return unify(x,y,nil)}function unify (x,y,bl) {if (x == y) {return bl};  if (varp(x)) {return unifyvar(x,y,bl)};  if (symbolp(x)) {return unifyatom(x,y,bl)};  return unifyexp(x,y,bl)}function unifyvar (x,y,bl) {var dum = assoc(x,bl);  if (dum != false) {return unify(cdr(dum),y,bl)};  if (x == unifyval(y,bl)) {return bl};  return acons(x,y,bl)}function unifyval (y,bl) {if (varp(y))     {var dum = assoc(y,bl);      if (dum != false) {return unifyval(cdr(dum),bl)};      return y};  return y}function unifyatom (x,y,bl) {if (varp(y)) {return unifyvar(y,x,bl)} else return false}function unifyexp(x,y,bl) {if (varp(y)) {return unifyvar(y,x,bl)}  if (symbolp(y)) {return false};  if (x.length != y.length) {return false};  for (var i=0; i<x.length; i++)      {bl = unify(x[i],y[i],bl);       if (bl == false) {return false}};  return bl}

//------------------------------------------------------------------------------// Plugging
//------------------------------------------------------------------------------

function plug (x,bl) {if (varp(x)) {return plugvar(x,bl)};  if (symbolp(x)) {return x};  return plugexp(x,bl)}function plugvar (x,bl) {var dum = assoc(x,bl);  if (dum == false) {return x};  return plug(cdr(dum),bl)}function plugexp (x,bl) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = plug(x[i],bl)};  return exp}var alist;function standardize (x) {alist = nil;  return standardizeit(x)}function standardizeit (x) {if (varp(x)) {return standardizevar(x)};  if (symbolp(x)) {return x};  return standardizeexp(x)}function standardizevar (x) {var dum = assoc(x,alist);  if (dum != false) {return cdr(dum)};  var rep = newvar();  alist = acons(x,rep,alist);  return rep}function standardizeexp (x) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = standardizeit(x[i])};  return exp}

//------------------------------------------------------------------------------// binding lists//------------------------------------------------------------------------------

function getbdg (x,al)
 {return al[x]}

function setbdg (x,al,y,bl)
 {var bdg = seq(y,bl);
  al[x] = bdg;
  return bdg}

function backup (bl)
 {for (var i=0; i<bl.length; i++) {bl[i].length = 0}}

//------------------------------------------------------------------------------// maatcher//------------------------------------------------------------------------------function maatcher (x,y) {return maatchify(x,seq(),y,seq(),seq())}function maatchify (x,al,y,bl,ol) {if (varp(x)) {return maatchvar(x,al,y,bl,ol)};  if (symbolp(x)) {return (x == y)};  return maatchexp(x,al,y,bl,ol)}function maatchvar (x,al,y,bl,ol) {if (x == y && al == bl) {return true};
  var dum = al[x];  if (dum && dum.length > 0) {return maatchify(dum[0],dum[1],y,bl,ol)};  if (x == maatchval(y,bl)) {return true};  ol[ol.length] = setbdg(x,al,y,bl);
  return true}function maatchval (y,bl) {if (varp(y))     {var dum = bl[y];      if (dum != null && dum[1] != nil) {return maatchval(dum[0],dum[1])};      return y};  return y}function maatchexp(x,al,y,bl,ol) {if (symbolp(y)) {return false};  var m = x.length;  var n = y.length;    if (m != n) {return false};  for (var i=0; i<m; i++)      {if (!maatchify(x[i],al,y[i],bl,ol)) {return false}};  return true}

//------------------------------------------------------------------------------// Unifier
//------------------------------------------------------------------------------

function vnifier (x,y) {var ol = seq();
  if (vnify(x,seq(),y,seq(),ol)) {return ol};
  backup(ol);
  return false}function vnifyp (x,al,y,bl,ol) {if (vnify(x,al,y,bl,ol)) {return ol};
  backup(ol);
  return false}function vnify (x,al,y,bl,ol)
 {if (varp(x)) {return vnifyvar(x,al,y,bl,ol)};  if (symbolp(x)) {return vnifyatom(x,al,y,bl,ol)};  return vnifyexp(x,al,y,bl,ol)}
function vnifyvar (x,al,y,bl,ol) {if (x == y && al == bl) {return true};  var dum = al[x];  if (dum && dum.length > 0) {return vnify(dum[0],dum[1],y,bl,ol)};  //if (x == vnifyval(y,bl)) {return true};  ol[ol.length] = setbdg(x,al,y,bl);
  return true}

function vnifyval (y,bl) {if (varp(y))     {var dum = bl[y];      if (dum != null && dum[1] != nil) {return vnifyval(dum[0],dum[1])};      return y};  return y}
function vnifyatom (x,al,y,bl,ol) {if (x == y) {return true};
  if (varp(y)) {return vnifyvar(y,bl,x,al,ol)};
  return false}function vnifyexp(x,al,y,bl,ol) {if (varp(y)) {return vnifyvar(y,bl,x,al,ol)}  if (symbolp(y)) {return false};  if (x.length != y.length) {return false};  for (var i=0; i<x.length; i++)      {if (!vnify(x[i],al,y[i],bl,ol)) {return false}};  return true}

//------------------------------------------------------------------------------// pluugging
//------------------------------------------------------------------------------

function pluug (x,al,bl)
 {if (varp(x)) {return pluugvar(x,al,bl)};  if (symbolp(x)) {return x};  return pluugexp(x,al,bl)}function pluugvar (x,al,bl) {var dum = al[x];  if (dum && dum.length > 0) {return pluug(dum[0],dum[1],bl)};
  if (al == bl) {return x};
  var rep = newvar();
  al[x] = seq(rep,bl);  return rep};function pluugexp (x,al,bl) {var exp = new Array(x.length);  for (var i=0; i<x.length; i++)      {exp[i] = pluug(x[i],al,bl)};  return exp}//------------------------------------------------------------------------------
// Storage
//------------------------------------------------------------------------------

var indexing = 'fullindexing'
var dataindexing = false
var ruleindexing = false

function definetheory (source,data)
 {emptytheory(source);
  definemore(source,data);
  return true}

function definemore (theory,data)
 {for (var i=0; i<data.length; i++) {insert(data[i],theory)};
  return true}

function definemorerules (theory,data)
 {for (var i=0; i<data.length; i++) {insertrule(data[i],theory)};
  return theory}

function emptytheory (theory)
 {theory.splice(0,theory.length);
  for (var x in theory) {delete theory[x]};
  return true}

//------------------------------------------------------------------------------

function drop (p,theory)
 {data = indexps(p,theory);
  for (var i=0; i<data.length; i++)
      {if (equalp(data[i],p)) {uninsert(data[i],theory); return data[i]}};
  return false}

function eliminate (object,theory)
 {var data = indexps(object,theory).concat();
  for (var i=0; i<data.length; i++)
      {if (data[i][1] == object) {uninsert(data[i],theory)}};
  return object}

//------------------------------------------------------------------------------

function insert (p,theory)
 {addcontent(p,theory);
  if (indexing) {index(p,p,theory)};
  return p}

function insertrule (p,theory)
 {addcontent(p,theory);
  if (ruleindexing) {relindex(p,p,theory)};
  return p}

function addcontent (p,theory)
 {theory.push(p);
  return p}

function index (x,p,theory)
 {if (varp(x)) {return p};
  if (symbolp(x)) {return indexsymbol(x,p,theory)};
  for (var i=0; i<x.length; i++) {index(x[i],p,theory)};
  return p}

function relindex (x,p,theory)
 {if (varp(x)) {return p};
  if (symbolp(x)) {return indexsymbol(x,p,theory)};
  if (x[0] == 'rule') {return relindex(x[1],p,theory)};
  return relindex(x[0],p,theory)}

function indexsymbol (x,p,theory)
 {if (x == 'map') {return p};
  if (!isNaN(Number(x))) {return p};
  var data = theory[x];
  if (data) {data.push(p)} else {theory[x] = seq(p)};
  return p}


function uninsert(p,theory)
 {if (indexing) {unindex(p,p,theory)};
  return remcontent(p,theory)}

function remcontent (p,theory)
 {for (var i=0; i<theory.length; i++)
      {if (theory[i]==p) {theory.splice(i,1); return p}};
  return false}

function unindex (x,p,theory)
 {if (varp(x)) {return p};
  if (symbolp(x)) {return unindexsymbol(x,p,theory)};
  for (var i=0; i<p.length; i++) {unindex(x[i],p,theory)};
  return p}

function unindexsymbol (x,p,theory)
 {if (theory[x]) {return remcontent(p,theory[x])} else {alert(x)}}


function indexps (p,theory)
 {if (indexing) {return fullindexps(p,theory)} else {return theory}}

function fullindexps (p,theory)
 {if (varp(p)) {return theory};
  if (symbolp(p)) {return indexees(p,theory)};
  for (var i=1; i<p.length; i++)
      {if (symbolp(p[i]) && !varp(p[i])) {return indexees(p[i],theory)}};
  return indexees(p[0],theory)}

function indexees (x,theory)
 {if (x != 'map' && isNaN(Number(x)))
     {var data = theory[x];
      if (data) {return data} else {return seq()}};
  return theory}

function envindexps (p,al,theory)
 {if (indexing) {return dataindexps(p,al,theory)} else {return theory}}

function dataindexps (p,al,theory)
 {if (!indexing) {return theory};
  if (varp(p)) {return theory};
  if (symbolp(p)) {return indexees(p,theory)};
  for (var i=1; i<p.length; i++)
      {var dum = unival(p[i],al);
       if (symbolp(dum) && !varp(dum)) {return indexees(dum,theory)}};
  return indexees(p[0],theory)}

function unival (x,al)
 {if (!varp(x)) {return x};
  var dum = assoc(x,al);
  if (dum) {return unival(cdr(dum),al)};
  return x}

function ruleindexps (p,theory)
 {if (!ruleindexing) {return theory};
  if (varp(p)) {return theory};
  if (symbolp(p)) {return indexees(p,theory)};
  if (p[0] == 'rule') {return ruleindexps(p[1],theory)};
  return indexees(p[0],theory)}

//------------------------------------------------------------------------------

function uniquify (ins)
 {var outs = seq();
  for (var i=0; i<ins.length; i++) {outs = adjoinit(ins[i],outs)};
  return outs}

function adjoinit (x,s) {if (find(x,s)) {return s} else {return concatenate(s,seq(x))}}

function nconc (l1,l2)
 {for (var i=0; i<l2.length; i++) {l1.push(l2[i])};
  return l1}

//------------------------------------------------------------------------------// viewfindp
// viewfindx
// viewfinds//------------------------------------------------------------------------------var thing;var answer;var answers;

function viewfindp (query,facts,rules) {return viewfindx('true',query,facts,rules)}function viewfindx (result,query,facts,rules) {thing = result;  answer = false;  if (proone(query,seq(),nil,facts,rules)) {return answer};  return false}function viewfinds (result,query,facts,rules) {thing = result;  answers = seq();  proall(query,seq(),nil,facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------// findp, findx, finds//------------------------------------------------------------------------------var database = seq();var rulebase = seq();function findp (query) {return findx('true',query)}function findx (result,query) {thing = result;  answer = false;  if (proone(query,seq(),nil,database,rulebase)) {return answer};  return false}function finds (result,query) {thing = result;  answers = seq();  proall(query,seq(),nil,database,rulebase);  return answers}//------------------------------------------------------------------------------var epitrace = false;
function proone (p,pl,al,facts,rules) {if (epitrace) {alert(grind(p) + '-' + pl + '-' + al)};  if (symbolp(p)) {return prooneatom(p,pl,al,facts,rules)}  if (p[0] == 'same') {return proonesame(p,pl,al,facts,rules)}  if (p[0] == 'distinct') {return proonedistinct(p,pl,al,facts,rules)}  if (p[0] == 'not') {return proonenot(p,pl,al,facts,rules)}  if (p[0] == 'and') {return prooneand(p,pl,al,facts,rules)}  if (p[0] == 'or') {return prooneor(p,pl,al,facts,rules)}  if (proonebackground(p,pl,al,facts,rules)) {return true};  return prooners(p,pl,al,facts,rules)}

function prooneatom (p,pl,al,facts,rules) {if (p == 'true') {return prooneexit(pl,al,facts,rules)};
  if (p == 'false') {return false};
  return prooners(p,pl,al,facts,rules)}
function proonenot (p,pl,al,facts,rules) {if (proone(p[1],seq(),al,facts,rules) == false) {return prooneexit(pl,al,facts,rules)}  return false}function prooneand (p,pl,al,facts,rules) {return prooneexit(concatenate(tail(p),pl),al,facts,rules)}function prooneor (p,pl,al,facts,rules) {var bl;  for (var i=1; i<p.length; i++)      {if (proone(p[i],pl,al,facts,rules)) {return true}}  return false}function proonesame (p,pl,al,facts,rules) {al = unify(p[1],p[2],al);  if (al != false) {return prooneexit(pl,al,facts,rules)};  return false}function proonedistinct (p,pl,al,facts,rules) {if (unify(p[1],p[2],al) == false) {return prooneexit(pl,al,facts,rules)};  return false}

function proonebackground (p,pl,al,facts,rules) {var bl;  var data = envindexps(p,al,facts);  for (var i=0; i<data.length; i++)      {bl = unify(data[i],p,al);       if (bl != false && prooneexit(pl,bl,facts,rules)) {return true}};  return false}function prooners (p,pl,al,facts,rules) {var copy;  var bl;  for (var i=0; i<rules.length; i++)      {copy = standardize(rules[i]);       if (copy[0] == 'rule')          {bl = unify(copy[1],p,al);           if (bl != false && proone(copy[2],concatenate(copy.slice(3),pl),bl,facts,rules))
              {return true}}          else {bl = unify(copy,p,al);                if (bl != false && prooneexit(pl,bl,facts,rules))
                   {return true}}};  return false}function prooneexit (pl,al,facts,rules) {if (pl.length != 0) {return proone(pl[0],tail(pl),al,facts,rules)};  answer = plug(thing,al);  return true}//------------------------------------------------------------------------------

function proall (p,pl,al,facts,rules) {if (epitrace) {alert(grind(p) + '-' + pl + '-' + al)};  if (symbolp(p)) {return proallatom(p,pl,al,facts,rules)}  if (p[0] == 'same') {return proallsame(p,pl,al,facts,rules)}  if (p[0] == 'distinct') {return proalldistinct(p,pl,al,facts,rules)}  if (p[0] == 'matches') {return proallmatches(p,pl,al,facts,rules)}  if (p[0] == 'not') {return proallnot(p,pl,al,facts,rules)}  if (p[0] == 'and') {return proalland(p,pl,al,facts,rules)}  if (p[0] == 'or') {return proallor(p,pl,al,facts,rules)}  proallbackground(p,pl,al,facts,rules);  return proallrs(p,pl,al,facts,rules)}

function proallatom (p,pl,al,facts,rules) {if (p == 'true') {return proallexit(pl,al,facts,rules)};
  if (p == 'false') {return false};
  return proallrs(p,pl,al,facts,rules)}function proallsame (p,pl,al,facts,rules) {al = unify(p[1],p[2],al);  if (al != false) {proallexit(pl,al,facts,rules)}}function proalldistinct (p,pl,al,facts,rules) {if (unify(p[1],p[2],al) == false) {proallexit(pl,al,facts,rules)}}function proallmatches (p,pl,al,facts,rules) {if (symbolp(p[1]))
     {var matches = p[1].match(p[2]);
      for (var i=0; i<matches.length; i++)
          {var bl = unify(p[3],matches[i],al);           if (bl != false) {proallexit(pl,bl,facts,rules)}}}
  return false}function proallnot (p,pl,al,facts,rules) {if (proone(p[1],seq(),al,facts,rules) == false) {proallexit(pl,al,facts,rules)}}function proalland (p,pl,al,facts,rules) {proallexit(concatenate(tail(p),pl),al,facts,rules)}function proallor (p,pl,al,facts,rules) {for (var i=0; i<p.length; i++) {proall(p[i],pl,al,facts,rules)}}function proallbackground (p,pl,al,facts,rules) {var bl;
  var data = envindexps(p,al,facts);  for (var i=0; i<data.length; i++)      {bl = match(p,data[i],al);       if (bl != false) {proallexit(pl,bl,facts,rules)}}}

function proallrs (p,pl,al,facts,rules) {var copy;  var bl;
  var data = ruleindexps(p,rules);
  for (var i=0; i<data.length; i++)      {copy = standardize(data[i]);       if (copy[0] == 'rule')          {bl = unify(copy[1],p,al);           if (bl != false) {proall(copy[2],concatenate(copy.slice(3),pl),bl,facts,rules)}}       else {bl = unify(copy,p,al);             if (bl != false) {proallexit(pl,bl,facts,rules)}}}}function proallexit (pl,bl,facts,rules) {if (pl.length != 0) {return proall(pl[0],tail(pl),bl,facts,rules)};  answers.push(plug(thing,bl))}

//------------------------------------------------------------------------------// compfindp
// compfindx
// compfinds//------------------------------------------------------------------------------

function compfindp (query,facts,rules) {return compfindx('true',query,facts,rules)}function compfindx (result,query,facts,rules) {thing = result;  answer = false;  if (compone(query,seq(),seq(),nil,facts,rules)) {return answer};  return false}function compfinds (result,query,facts,rules) {thing = result;  answers = seq();  compall(query,seq(),seq(),nil,facts,rules);  return uniquify(answers)}

//------------------------------------------------------------------------------

function compone (p,pl,al,cont,facts,rules) {if (epitrace) {alert(grind(p) + '-' + pl + '-' + al)};  if (symbolp(p)) {return componeatom(p,pl,al,cont,facts,rules)}  if (p[0] == 'same') {return componesame(p,pl,al,cont,facts,rules)}  if (p[0] == 'distinct') {return componedistinct(p,pl,al,cont,facts,rules)}  if (p[0] == 'matches') {return componematches(p,pl,al,cont,facts,rules)}  if (p[0] == 'not') {return componenot(p,pl,al,cont,facts,rules)}  if (p[0] == 'and') {return componeand(p,pl,al,cont,facts,rules)}  if (p[0] == 'or') {return componeor(p,pl,al,cont,facts,rules)}  if (componebackground(p,pl,al,cont,facts,rules)) {return true};  return componers(p,pl,al,cont,facts,rules)}

function componeatom (p,pl,al,cont,facts,rules) {if (p == 'true') {return componeexit(pl,al,cont,facts,rules)};
  if (p == 'false') {return false};
  return componers(p,pl,al,cont,facts,rules)}function componesame (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {return componeexit(pl,al,cont,facts,rules)};
  backup(ol);
  return false}function componedistinct (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};
  return componeexit(pl,al,cont,facts,rules)}function componematches (p,pl,al,cont,facts,rules) {if (symbolp(p[1]))
     {var matches = p[1].match(p[2]);
      for (var i=0; i<matches.length; i++)
          {var ol = seq();
           if (vnifyp(p[3],al,matches[i],al,ol))
              {if (componeexit(pl,bl,cont,facts,rules))
                  {backup(ol); return true}}}};
  return false}function componenot (p,pl,al,cont,facts,rules) {if (!compone(p[1],seq(),al,nil,facts,rules))
     {return componeexit(pl,al,cont,facts,rules)};
  return false}function componeand (p,pl,al,cont,facts,rules) {return componeexit(concatenate(tail(p),pl),al,cont,facts,rules)}function componeor (p,pl,al,cont,facts,rules) {for (var i=0; i<p.length; i++)
      {if (compone(p[i],pl,al,cont,facts,rules)) {return true}};
  return false}

function componebackground (p,pl,al,cont,facts,rules) {var data = facts;
  for (var i=0; i<data.length; i++)      {var bl = seq();
       var ol = seq();
       if (vnifyp(data[i],bl,p,al,ol))
          {if (componeexit(pl,al,cont,facts,rules)) {backup(ol); return true};
           backup(ol)}}
  return false}
function componers (p,pl,al,cont,facts,rules) {var data = ruleindexps(p,rules);
  for (var i=0; i<data.length; i++)      {var bl = seq();
       var ol = seq();
       if (data[i][0] == 'rule')          {if (vnifyp(data[i][1],bl,p,al,ol))
              {var ql = data[i].slice(3);
               var nc = cons(seq(pl,al,cont),cont);
               if (compone(data[i][2],ql,bl,nc,facts,rules))
                  {backup(ol); return true};
               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))
                {if (componeexit(pl,al,cont,facts,rules))
                    {backup(ol); return true};
                 backup(ol)}}}
  return false}

function componeexit (pl,al,cont,facts,rules) {if (pl.length != 0) {return compone(pl[0],tail(pl),al,cont,facts,rules)};
  if (nullp(cont)) {answer = pluug(thing,al,al); return true};
  return componeexit(car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}

//------------------------------------------------------------------------------

function compall (p,pl,al,cont,facts,rules) {if (epitrace) {alert(grind(pluug(p,al,al)) + '-' + pl + '-' + al)};  if (symbolp(p)) {return compallatom(p,pl,al,cont,facts,rules)}  if (p[0] == 'same') {return compallsame(p,pl,al,cont,facts,rules)}  if (p[0] == 'distinct') {return compalldistinct(p,pl,al,cont,facts,rules)}  if (p[0] == 'matches') {return compallmatches(p,pl,al,cont,facts,rules)}  if (p[0] == 'not') {return compallnot(p,pl,al,cont,facts,rules)}  if (p[0] == 'and') {return compalland(p,pl,al,cont,facts,rules)}  if (p[0] == 'or') {return compallor(p,pl,al,cont,facts,rules)}  compallbackground(p,pl,al,cont,facts,rules);  return compallrs(p,pl,al,cont,facts,rules)}

function compallatom (p,pl,al,cont,facts,rules) {if (p == 'true') {return compallexit(pl,al,cont,facts,rules)};
  if (p == 'false') {return false};
  return compallrs(p,pl,al,cont,facts,rules)}function compallsame (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))
     {compallexit(pl,al,cont,facts,rules); backup(ol)}}function compalldistinct (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};
  return compallexit(pl,al,cont,facts,rules)}function compallmatches (p,pl,al,cont,facts,rules) {if (symbolp(p[1]))
     {var matches = p[1].match(p[2]);
      for (var i=0; i<matches.length; i++)
          {var ol = seq();
           if (vnifyp(p[3],al,matches[i],al,ol))
              {compallexit(pl,bl,cont,facts,rules); backup(ol)}}}
  return false}function compallnot (p,pl,al,cont,facts,rules) {if (compone(p[1],seq(),al,nil,facts,rules) == false) {compallexit(pl,al,cont,facts,rules)}}function compalland (p,pl,al,cont,facts,rules) {compallexit(concatenate(tail(p),pl),al,cont,facts,rules)}function compallor (p,pl,al,cont,facts,rules) {for (var i=0; i<p.length; i++) {compall(p[i],pl,al,cont,facts,rules)}}

function compallbackground (p,pl,al,cont,facts,rules) {var data = facts;
  for (var i=0; i<data.length; i++)      {var bl = seq();
       var ol = seq();
       if (vnifyp(data[i],bl,p,al,ol))
          {compallexit(pl,al,cont,facts,rules);
           backup(ol)}}}
function compallrs (p,pl,al,cont,facts,rules) {var data = ruleindexps(p,rules);
  for (var i=0; i<data.length; i++)      {var bl = seq();
       var ol = seq();
       if (data[i][0] == 'rule')          {if (vnifyp(data[i][1],bl,p,al,ol))
              {var ql = data[i].slice(3);
               var nc = cons(seq(pl,al,cont),cont);
               compall(data[i][2],ql,bl,nc,facts,rules);
               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))
                {compallexit(pl,al,cont,facts,rules);
                 backup(ol)}}}}

function compallexit (pl,al,cont,facts,rules) {if (pl.length != 0) {return compall(pl[0],tail(pl),al,cont,facts,rules)};
  if (nullp(cont)) {answers.push(pluug(thing,al,al)); return true};
  return compallexit(car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}

//------------------------------------------------------------------------------// Input and Output//------------------------------------------------------------------------------function readdata (str) {return parsedata(scan(str))}function read (str) {return parse(scan(str))}function readitems (str) {return parseitems(scan(str))}//------------------------------------------------------------------------------var input = '';var output = '';var current = 0;function scan (str) {input = str;  output = new Array(0);  var cur = 0;  var len = input.length;  while (cur < len)   {var charcode = input.charCodeAt(cur);    if (charcode == 32 || charcode == 13) {cur++}    else if (charcode == 34) {cur = scanstring(cur)}    else if (charcode == 38) {output[output.length] = '&'; cur++}    else if (charcode == 40) {output[output.length] = 'lparen'; cur++}    else if (charcode == 41) {output[output.length] = 'rparen'; cur++}    else if (charcode == 43) {output[output.length] = '+'; cur++}    else if (charcode == 44) {output[output.length] = 'comma'; cur++}    else if (charcode == 45) {output[output.length] = '-'; cur++}    else if (charcode == 58) {cur = scanrulesym(cur)}    else if (charcode == 60) {cur = scanbacksym(cur)}    else if (charcode == 61) {cur = scanthussym(cur)}    else if (charcode == 62) {output[output.length] = '>'; cur++}    else if (charcode == 123) {output[output.length] = '{'; cur++}    else if (charcode == 124) {output[output.length] = '|'; cur++}    else if (charcode == 125) {output[output.length] = '}'; cur++}    else if (charcode == 126) {output[output.length] = '~'; cur++}    else if (idcharp(charcode)) {cur = scansymbol(cur)}    else cur++};  return output}function scanrulesym (cur) {if (input.length > cur+1 && input.charCodeAt(cur+1) == 45)     {output[output.length] = ':-'; return cur+2};  if (input.length > cur+1 && input.charCodeAt(cur+1) == 61)     {output[output.length] = ':='; return cur+2}  else {output[output.length] = ':'; return cur+1}}function scanbacksym (cur) {if (input.length > cur+1 && input.charCodeAt(cur+1) == 61)     {if (input.length > cur+2 && input.charCodeAt(cur+2) == 62)         {output[output.length] = '<=>'; return cur+3}      else {output[output.length] = '<='; return cur+2}}  else {output[output.length] = '<'; return cur+1}}function scanthussym (cur) {if (input.length > cur+1 && input.charCodeAt(cur+1) == 62)     {output[output.length] = '=>'; return cur+2}  else {output[output.length] = '='; return cur+1}}function scansymbol (cur) {var n = input.length;  var exp = '';  while (cur < n)   {if (idcharp(input.charCodeAt(cur))) {exp = exp + input[cur]; cur++}    else break};  if (exp != '') {output[output.length] = exp};  return cur}

function scanstring (cur) {var exp = '"';
  cur++;  while (cur < input.length)
   {exp = exp + input[cur];;    if (input.charCodeAt(cur) == 34) {cur++; break}
    cur++};
  output[output.length] = exp;  return cur}
function letterp (charcode)
 {return ((charcode >= 65 && charcode <= 90) ||
          (charcode >= 97 && charcode <= 122))}function idcharp (charcode) {if (charcode == 46) {return true};  if (charcode >= 48 && charcode <= 57) {return true};  if (charcode >= 65 && charcode <= 90) {return true};  if (charcode >= 97 && charcode <= 122) {return true};  if (charcode == 95) {return true};  return false}//------------------------------------------------------------------------------

function parsedata (str) {str.push('eof');  input = str;  current = 0;  exp = new Array(0);  while (current < input.length && input[current] != 'eof')   {exp[exp.length] = parsexp('lparen','rparen')};  return exp}function parseitems (str) {str.push('eof');  input = str;  current = 0;  exp = seq();  while (current < input.length && input[current] != 'eof')   {if (input[current] == 'comma') {current++};
    exp[exp.length] = parsexp('lparen','rparen')};  return exp}

function parse (str) {str.push('eof');  input = str;  current = 0;  return parsexp('lparen','rparen')}function parsexp (lop,rop) {var left = parseprefix(rop);  while (current < input.length)   {if (input[current] == 'eof') {return left}    else if (input[current] == 'lparen') {left = parseatom(left)}    else if (input[current] == '.') {current++; return(left)}    else if (precedencep(lop,input[current])) {return left}    else {left = parseinfix(left,input[current],rop)}};  return left}function parseprefix (rop) {var left = input[current];  current++;  if (left == 'lparen') {left = parsexp('lparen','rparen'); current++; return left};  if (left == '~') {return makenegation(parsexp('~',rop))};  if (left == '{') {return parseclause()};  return left}function parseatom (left) {var exp = parseparenlist();  exp.unshift(left);  return exp}function parseparenlist () {var exp = new Array(0);  current++;  if (input[current] == 'rparen') {current++; return exp};  while (current < input.length)   {exp.push(parsexp('comma','rparen'));    if (input[current] == 'rparen') {current++; return exp};    if (input[current] == 'comma') {current++} else {return exp}};  return exp}function parseclause () {var exp = seq('clause');  while (current < input.length)   {exp.push(parsexp('comma','rparen'));    if (input[current] == '}') {current++; return exp};    if (input[current] == 'comma') {current++}    else {return exp}};  return exp}function parseinfix (left,op,rop) {if (op == ':') {return parsequantifier(left,rop)};  if (op == '&') {return parseand(left,rop)};  if (op == '|') {return parseor(left,rop)};  if (op == '<=>') {return parseequivalence(left,rop)};  if (op == '=>') {return parseimplication(left,rop)};  if (op == '<=') {return parsereduction(left,rop)};  if (op == ':-') {return parserule(left,rop)};  if (op == ':=') {return parsedefinition(left,rop)};  return left}function parsequantifier (left,rop) {current++;  if (left[0] == 'A') {return makeuniversal(left.slice(1,left.length),parsexp(':',rop))};  if (left[0] == 'E') {return makeexistential(left.slice(1,left.length),parsexp(':',rop))};  return makeuniversal(left,parsexp(':',rop))}function parseand (left,rop) {current++;  return makeconjunction(left,parsexp('&',rop))}function parseor (left,rop) {current++;  return makedisjunction(left,parsexp('|',rop))}function parseequivalence (left,rop) {current++;  var dum = parsexp('<=>',rop);  return makeequivalence(left,dum)}function parseimplication (left,rop) {current++;  var dum = parsexp('=>',rop);  return makeimplication(left,dum)}function parsereduction (left,rop) {current++;  var dum = parsexp('<=',rop);  return makereduction(left,dum)}
function parserule (left,rop) {current++;  var dum = parsexp(':-',rop);  return makerule(left,dum)}function parsedefinition (left,rop) {current++;  var dum = parsexp(':=',rop);  return makedefinition(left,dum)}function precedencep (lop,rop) {var dum = pp(lop,rop);  //alert(lop + '-' + rop + '-' + dum);  return dum}function pp (lop,rop) {if (lop == ':') {return  rop != ':'};  if (lop == '~') {return  rop != ':'};  if (lop == '&') {return rop != ':' && rop != '~'};  if (lop == '|') {return rop != ':' && rop != '~' && rop != '&'};  if (lop == '=>') {return rop != ':' && rop != '~' && rop != '&' && rop != '|'};  if (lop == '<=') {return rop != ':' && rop != '~' && rop != '&' && rop != '|'};  if (lop == '<=>') {return rop != ':' && rop != '~' && rop != '&' && rop != '|'};  if (lop == ':-') {return rop != ':' && rop != '~' && rop != '&' && rop != '|'};  if (lop == ':=') {return rop != ':' && rop != '~' && rop != '&' && rop != '|'};  return rop != ':' && rop != '~' && rop != '&' && rop != '|'                    && rop != '=>' && rop != '<=' && rop != '<=>'                    && rop != ':-' && rop != ':='}function parenp (lop,op,rop) {return precedencep(lop,op) || precedencep(rop,op)}//------------------------------------------------------------------------------
// readkifdata
// readkif
//------------------------------------------------------------------------------
function readkifdata (str)
 {return kifdata(kifscan(str))}

function readkif (str)
 {return kif(kifscan(str))}

function kifscan (str) {input = str;  output = new Array(0);  var cur = 0;  var len = input.length;  while (cur < len)   {var charcode = input.charCodeAt(cur);    if (charcode == 32 || charcode == 13) {cur++}    else if (charcode == 34) {cur = scanstring(cur)}    else if (charcode == 40) {output[output.length] = 'lparen'; cur++}    else if (charcode == 41) {output[output.length] = 'rparen'; cur++}    else if (charcode == 59) {cur = kifscancomment(cur)}    else if (charcode == 63) {cur = kifscanvariable(cur)}    else if (kifidcharp(charcode)) {cur = kifscansymbol(cur)}    else cur++};  return output}function kifscansymbol (cur) {var exp = '';  while (cur < input.length)   {if (kifidcharp(input.charCodeAt(cur))) {exp = exp + input[cur]; cur++}    else break};  if (exp != '') {output[output.length] = exp};  return cur}function kifscanvariable (cur) {cur++;
  var exp = '';
  if (letterp(input.charCodeAt(cur)))
     {exp=input.slice(cur,cur+1).toUpperCase(); cur++}
     else {exp = 'VV'};  while (cur < input.length)   {if (kifidcharp(input.charCodeAt(cur))) {exp = exp + input[cur]; cur++}    else break};  if (exp != '') {output[output.length] = exp};  return cur}

function kifscanstring (cur) {var exp = '';  cur++  while (cur < input.length && input.charCodeAt(cur) != 34)
        {exp = exp + input[cur]; cur++};
  cur++;
  output[output.length] = exp
  return cur}function kifscancomment (cur) {while (cur < input.length && input.charCodeAt(cur) != 10 && input.charCodeAt(cur) != 13) {cur++};
  return cur}function kifidcharp (charcode) {if (charcode == 45) {return true};  if (charcode == 46) {return true};  if (charcode == 60) {return true};  if (charcode == 61) {return true};  if (charcode >= 48 && charcode <= 57) {return true};  if (charcode >= 65 && charcode <= 90) {return true};  if (charcode >= 97 && charcode <= 122) {return true};  if (charcode == 95) {return true};  return false}

function kifdata (str)
 {str.push('eof');
  input = str;
  current = 0;
  exp = new Array(0);
  while (current < input.length && input[current] != 'eof')
   {exp[exp.length] = kifexp()};
  return exp}

function kif (str)
 {str.push('eof');
  input = str;
  current = 0;
  return kifexp()}

function kifexp ()
 {var lexeme = input[current];
  if (lexeme == 'eof') {return nil};
  if (lexeme == '<=') {current++; return 'rule'};
  if (lexeme == 'lparen') {return kifparenlist()};
  current++; return lexeme}

function kifparenlist ()
 {var exp = new Array(0);
  current++;
  if (input[current] == 'rparen') {current++; return exp};
  while (current < input.length)
   {exp.push(kifexp());
    if (input[current] == 'rparen') {current++; return exp}};
  return exp}

//------------------------------------------------------------------------------function printdata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + printit(data[i]) + '<br/>'}  return exp}function printem (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + printit(data[i]) + '\r'}  return exp}function printit (p) {if (p == 'rule') {return '<='};  if (varp(p)) {return '?' + p};  if (symbolp(p)) {return p};  var n = p.length;  var exp = '(';  if (n>0) {exp += printit(p[0])};  for (var i=1; i<n; i++)      {exp = exp + ' ' + printit(p[i])}  exp += ')';  return exp}//------------------------------------------------------------------------------function doxml () {var win = window.open();  //win.document.open('text/html');  win.document.writeln('&lt;?xml version="1.0"?&gt;<br/>\n');  win.document.writeln('&lt;?xml-stylesheet type="text/xsl" href="../stylesheets/proof.xsl"?&gt;<br/>\n');  win.document.write(xmlproof());  win.document.close()}function xmlproof () {var exp = '';  exp += '&lt;proof&gt;<br/>\n';  for (var i=1; i<proof.length; i++)      {exp += '  &lt;step&gt;<br/>';       exp += '    &lt;number&gt;' + i + '&lt;/number&gt;<br/>\n';       exp += '    &lt;sentence&gt;' + grind(proof[i][1]) + '&lt;/sentence&gt;<br/>\n';       exp += '    &lt;justification&gt;' + prettify(proof[i][2]) + '&lt;/justification&gt;<br/>\n';       for (var j=3; j<proof[i].length; j++)           {exp += '    &lt;antecedent&gt;' + proof[i][j] + '&lt;/antecedent&gt;<br/>\n'};       exp += '  &lt;/step&gt;<br/>\n'};  exp += '&lt;/proof&gt;<br/>\n';  return exp}function xmlify (str) {str = str.replace('&','&amp;');  str = str.replace('<=>','&lt;=&gt;');  return str}//------------------------------------------------------------------------------function smoothdata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + smooth(data[i]) + '<br/>'}  return exp}function smooth (p) {if (symbolp(p)) {return p};  var exp = p[0] + '(';  if (p.length > 1) {exp += smooth(p[1])};  for (var i=2; i<p.length; i++)      {exp += ',' + smooth(p[i])}  exp += ')';  return exp}//------------------------------------------------------------------------------function grindproof (proof) {var exp = '';  exp = exp + '<table cellpadding="4" cellspacing="0" border="1">';  exp = exp + '<tr bgcolor="#bbbbbb">';  exp = exp + '<td>&nbsp;</td>'; //exp = exp + '<td><input type="checkbox" name="Selection"/></td>';  exp = exp + '<th>Step</th><th>Proof</th><th>Justification</th>';  exp = exp + '</tr>';  for (var i=0; i<proof.length; i=i+3)      {exp = exp + '<tr id="0">';       exp = exp + '<td bgcolor="#eeeeee"><input id="' + (i/3 + 1) + '" type="checkbox"/></td>';       exp = exp + '<td align="center" bgcolor="#eeeeee">' + (i/3 + 1) + '</td>';       exp = exp + '<td>' + grind(proof[i+1]) + '</td>';       exp = exp + '<td bgcolor="#eeeeee">' + proof[i+2] + '</td>';       exp = exp + '</tr>'};         exp = exp + '</table>';  return exp}//------------------------------------------------------------------------------function grinddata (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + grind(data[i]) + '<br/>'}  return exp}function grindem (data) {var exp = '';  var n = data.length;  for (var i=0; i<n; i++)      {exp = exp + grind(data[i]) + '\r'}  return exp}function grind (p) {return grindit(p,'lparen','rparen')}function grindit (p,lop,rop) {if (symbolp(p)) {return p};  if (p[0] == 'definition') {return grinddefinition(p,lop,rop)};  if (p[0] == 'not') {return grindnegation(p,rop)};  if (p[0] == 'and') {return grindand(p,lop,rop)};  if (p[0] == 'or') {return grindor(p,lop,rop)};  if (p[0] == 'equivalence') {return grindequivalence(p,lop,rop)};  if (p[0] == 'implication') {return grindimplication(p,lop,rop)};  if (p[0] == 'reduction') {return grindreduction(p,lop,rop)};  if (p[0] == 'rule') {return grindrule(p,lop,rop)};  if (p[0] == 'clause') {return grindclause(p)};  if (p[0] == 'forall') {return grinduniversal(p,lop,rop)};  if (p[0] == 'exists') {return grindexistential(p,lop,rop)};  return grindatom(p)}function grindatom (p) {var n = p.length;  var exp = p[0] + '(';  if (n>1) {exp += grind(p[1])};  for (var i=2; i<n; i++)      {exp = exp + ',' + grind(p[i])}  exp += ')';  return exp}function grinddefinition (p,lop,rop) {var exp = '';  var parens = parenp(lop,':=',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,':=') + ' := ' + grindit(p[2],':=',rop);  if (parens) {exp = exp + ')'};  return exp}function grindnegation (p,rop) {return '~' + grindit(p[1],'~',rop)}function grindand (p,lop,rop) {if (p.length == 1) {return 'true'};  if (p.length == 2) {return grind(p[1],lop,rop)};  var exp;  exp = grindleft(lop,'&',rop) + grindit(p[1],lop,'&');  for (var i=2; i<p.length-1; i++)      {exp = exp + ' & ' + grindit(p[i],'&','&')};  exp = exp + ' & ' + grindit(p[p.length-1],'&',rop) + grindright(lop,'&',rop);  return exp}function grindor (p,lop,rop) {var exp;  if (p.length == 1) {return 'false'};  if (p.length == 2) {return grind(p[1],lop,rop)};  exp = grindleft(lop,'|',rop) + grindit(p[1],lop,'|');  for (var i=2; i<p.length-1; i++)      {exp = exp + ' | ' + grindit(p[i],'|','|')};  exp = exp + ' | ' + grindit(p[p.length-1],'|',rop) + grindright(lop,'|',rop);  return exp}function grindequivalence (p,lop,rop) {var exp = '';  var parens = parenp(lop,'<=>',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'<=>') + ' <=> ' + grindit(p[2],'<=>',rop);  if (parens) {exp = exp + ')'};  return exp}function grindimplication (p,lop,rop) {var exp = '';  var parens = parenp(lop,'=>',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'=>') + ' => ' + grindit(p[2],'=>',rop);  if (parens) {exp = exp + ')'};  return exp}function grindreduction (p,lop,rop) {var exp = '';  var parens = parenp(lop,'<=',rop);  if (parens) {lop = 'lparen'; rop = 'rparen'};  if (parens) {exp = '('};  exp = exp + grindit(p[1],lop,'<=') + ' <= ' + grindit(p[2],'<=',rop);  if (parens) {exp = exp + ')'};  return exp}function grindrule (p,lop,rop) {var exp = grind(p[1]) + ' :- ';  if (p.length == 2) {exp += 'true'}  else if (p.length == 3) {exp += grindit(p[2],':-',rop)}  else {exp += grindit(p[2],lop,'&');        for (var i=3; i<p.length-1; i++)            {exp = exp + ' & ' + grindit(p[i],'&','&')};        exp += ' & ' + grindit(p[p.length-1],'&',rop)};  return exp}function grindclause (p) {var exp = '{';  if (p.length > 1) {exp = exp + grind(p[1])};  for (var i=2; i<p.length; i++)      {exp = exp + ',' + grind(p[i])};  exp = exp + '}';  return exp}function grinduniversal (p,lop,rop) {return grindleft(lop,':',rop) + 'A' + grindit(p[1],lop,':') + ':' + grindit(p[2],':',rop) + grindright(lop,':',rop)}function grindexistential (p,lop,rop) {return grindleft(lop,':',rop) + 'E' + grindit(p[1],lop,':') + ':' + grindit(p[2],':',rop) + grindright(lop,':',rop)}function grindleft (lop,op,rop) {if (precedencep(lop,op) || precedencep(rop,op)) {return "("}  return ""}function grindright (lop,op,rop) {if (precedencep(lop,op) || precedencep(rop,op)) {return ")"}  return ""}function grindalist (al) {var exp = '';  if (al == false) {return 'false'};  for (var l=al; !nullp(l); l=cdr(l))      {exp = exp + car(car(l)) + ' = ' + grind(cdr(car(l))) + '<br/>'}  return exp}//------------------------------------------------------------------------------function displayproof (proof) {var exp = '';  exp = exp + '<table cellpadding="4" cellspacing="0" border="1">';  exp = exp + '<tr bgcolor="#bbbbbb">';  exp = exp + '<td><input type="checkbox" onClick="doselectall()"/></td>';  exp = exp + '<th>Step</th><th>Proof</th><th>Justification</th>';  exp = exp + '</tr>';  for (var i=1; i<proof.length; i++)      {exp = exp + '<tr id="0">';       exp = exp + '<td bgcolor="#eeeeee"><input id="' + i +
                   '" type="checkbox"/></td>';       exp = exp + '<td align="center" bgcolor="#eeeeee">' + i + '</td>';       exp = exp + '<td>' + grind(proof[i][1]) + '</td>';       exp += '<td bgcolor="#eeeeee">';       exp += prettify(proof[i][2]);       if (proof[i].length > 3)          {exp += ': ' + proof[i][3];           for (var j=4; j<proof[i].length; j++) {exp += ', ' + proof[i][j]}};       exp += '</td>';       exp = exp + '</tr>'};         exp = exp + '</table>';  return exp}function prettify (str) {return str.replace('_',' ')}//------------------------------------------------------------------------------// End//------------------------------------------------------------------------------