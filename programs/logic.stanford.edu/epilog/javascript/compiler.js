//------------------------------------------------------------------------------// compiler.js//------------------------------------------------------------------------------
//------------------------------------------------------------------------------// compile// compiledcode
// decompile//------------------------------------------------------------------------------

function compile (rules)
 {var views = getviews(rules);
  for (var i=0; i<views.length; i++)
      {console.log(views[i]);
eval.call(window,grindjs(compileview(views[i],rules)))};
  var bases = getbasesfromrules(rules);
  for (var i=0; i<bases.length; i++)
      {eval.call(window,grindjs(compilebase(bases[i],rules)))};
  return true}

function compiledcode (rules)
 {var code = '';
  var views = getviews(rules);
  for (var i=0; i<views.length; i++)
      {code += grindjs(compileview(views[i],rules)) + '\r'};
  var bases = getbasesfromrules(rules);
  for (var i=0; i<bases.length; i++)
      {code += grindjs(compilebase(bases[i],rules)) + '\r'};
  return code}

function decompile (rules)
 {var views = getviews(rules);
  for (var i=0; i<views.length; i++)
      {window['comp' + views[i]] = null};
  var bases = getbasesfromrules(rules);
  for (var i=0; i<bases.length; i++)
      {window['comp' + bases[i]] = null};
  return true}

//------------------------------------------------------------------------------

function compilebase (rel,rules)
 {var arity = getarity(rel,rules);
  var params = makepattern(rel,arity).slice(1);
  return seq('function','comp' + rel,params,
             seq('return',seq('baseanswers',seq('quote',rel)).concat(params)))}

//------------------------------------------------------------------------------

function compileview (view,rules)
 {var data = normalize(indexees(view,rules));
  var code = seq('function','comp' + view,operands(data[0]));
  code.push(seq('bind','answers',seq('seq')));
  for (var i=0; i<data.length; i++)
      {var rulecode = compilerule(data[i]);
       if (rulecode) {code.push(rulecode)}};
  code.push(seq('return','answers'));
  return code}

function compilerule (rule)
 {var params = operands(rule);
  var alist = seq();
  for (var i=0; i<params.length; i++)
      {alist[params[i]] = params[i]};
  return compilesubgoals(rule,2,alist)};

function compilesubgoals (rule,n,alist)
 {if (n>=rule.length)
     {var answer = codify(rule[1],alist);
      return seq('block',seq('answers.push',answer))};
  return compilesubgoal(rule,n,alist)}

function compilesubgoal (rule,n,alist)
 {if (symbolp(rule[n])) {return compileatom(rule,n,alist)};
  if (rule[n][0]==='is') {return compileis(rule,n,alist)};
  if (rule[n][0]==='same') {return compilesame(rule,n,alist)};
  if (rule[n][0]==='distinct') {return compiledistinct(rule,n,alist)};
  if (rule[n][0]==='not') {return compilenot(rule,n,alist)};
  if (false && compgroundp(rule[n],alist)) {return compileground(rule,n,alist)};
  return compiledb(rule,n,alist)}

function compileatom (rule,n,alist)
 {if (rule[n]==='true') {return compilesubgoals(rule,n+1,alist)};
  if (rule[n]==='false') {return false};
  var cond = seq('viewfindp',seq('quote',rule[n]),'repository','library')
  return seq('if',cond,compilesubgoals(rule,n+1,alist))}

function compileis (rule,n,alist)
 {var cond = seq('simplematchp',rule[n][1],codify(rule[n][2],alist));
  alist[rule[n][1]] = codify(rule[n][2],alist);
  return seq('if',cond,compilesubgoals(rule,n+1,alist))}

function compilesame (rule,n,alist)
 {if (varp(rule[n][1]) || varp(rule[n][2]))
     {var x = codify(rule[n][1],alist);
      var y = codify(rule[n][2],alist);
      var cond = seq('equalp',x,y);
      return seq('if',cond,compilesubgoals(rule,n+1,alist))};
  if (rule[n][1]===rule[n][2]) {return compilesubgoals(rule,n+1,alist)};
  return false};

function compiledistinct (rule,n,alist)
 {if (varp(rule[n][1]) || varp(rule[n][2]))
     {var x = codify(rule[n][1],alist);
      var y = codify(rule[n][2],alist);
      var cond = seq('equalp',x,y);
      return seq('if',seq('not',cond),compilesubgoals(rule,n+1,alist))};
  if (rule[n][1]===rule[n][2]) {return false};
  return compilesubgoals(rule,n+1,alist)};

function compilenot (rule,n,alist)
 {var code = codify(rule[n][1],alist);
  var cond = seq('viewfindp',code,'repository','library');
  return seq('if',seq('not',cond),compilesubgoals(rule,n+1,alist))}

function compileground (rule,n,alist)
 {var code = codify(rule[n][1],alist);
  var cond = seq('viewfindp',code,'repository','library');
  return seq('if',cond,compilesubgoals(rule,n+1,alist))}

function compiledb (rule,n,alist)
 {var datavar = 'l'+(n-1);
  var indvar = 'i'+(n-1);
  var subgoal = compilers(rule[n],alist);
  bindvars(rule[n],seq('dot',datavar,indvar),alist);
  var code = compilesubgoals(rule,n+1,alist);
  if (!code) {return false};
  return seq('block',
             seq('bind',datavar,subgoal),
             seq('loop',indvar,'0',datavar+'.length',code))}

function compilers (subgoal,alist)
 {var answer = seq('comp' + subgoal[0]);
  for (var i=1; i<subgoal.length; i++)
      {answer.push(codify(subgoal[i],alist))};
  return answer}

function compgroundp (x,alist)
 {if (varp(x)) {return alist[x]!==null && alist[x]!==x};
  if (symbolp(x)) {return true};
  for (var i=1; i<x.length; i++)
      {if (!compgroundp(x[i],alist)) {return false}};
  return true}

function bindvars (x,code,alist)
 {if (varp(x))
     {var val = alist[x];
      if (!val || val===x) {alist[x] = code}};
  if (symbolp(x)) {return true};
  for (var i=1; i<x.length; i++)
      {bindvars(x[i],seq('dot',code,i),alist)};
  return true}

function codify (x,alist)
 {if (varp(x))
     {var val = alist[x];
      if (val) {return val} else return seq('quote',x)};
  if (symbolp(x)) {return seq('quote',x)};
  var exp = seq('seq');
  for (var i=0; i<x.length; i++)
      {exp.push(codify(x[i],alist))};
  return exp}

//------------------------------------------------------------------------------
// normalize
//------------------------------------------------------------------------------

function normalize (rules)
 {var results = seq();
  for (var i=0; i<rules.length; i++)
      {results.push(normalizerule(rules[i]))};
  return results}

function normalizerule (rule)
 {if (symbolp(rule)) {return seq('rule',rule)};
  if (rule[0]!=='rule')
     {var head = seq(operator(rule));
      var newrule = seq('rule',head)
      for (var i=1; i<rule.length; i++)
          {var headvar = 'V' + i;
           head.push(headvar);
           newrule.push(seq('is',headvar,rule[i]))};
      return newrule};
  if (symbolp(rule[1])) {return rule};

  var head = rule[1];
  var alist = seq();
  var hlist = seq();
  var newhead = seq(head[0]);
  for (var i=1; i<head.length; i++)
      {var headvar = 'V' + i;
       newhead.push(headvar);
       if (varp(head[i])) 
          {var dum = alist[head[i]];
           if (dum) {hlist[headvar] = dum}
              else {alist[head[i]] = headvar}}
          else {hlist[headvar] = head[i]}};

  var newrule = seq('rule',newhead)
  for (var i=2; i<rule.length; i++)
      {var tlist = seq();
       newrule.push(normalplug(rule[i],alist,tlist));
       for (var x in tlist)
           {for (var j=0; j<tlist[x].length; j++)
                {newrule.push(seq('same',tlist[x][j],x))}}};

  for (var x in hlist)
      {newrule.push(seq('same',x,ordplug(hlist[x],alist)))};
  return newrule}

function ordplug (x,alist)
 {if (varp(x))
     {var dum = alist[x];
      if (dum) {return dum} else {return x}}
  if (symbolp(x)) {return x};
  var exp = seq(x[0]);
  for (var i=1; i<x.length; i++)
      {exp.push(ordplug(x[i],alist))};
  return exp}

function normalplug (x,alist,blist)
 {if (varp(x))
     {var dum = alist[x];
      if (dum) {x = dum};
      dum = blist[x];
      if (dum) {var temp = newtemp(); dum.push(temp); return temp};
      blist[x] = seq();
      return x}
  if (symbolp(x)) {return x};
  var exp = seq(x[0]);
  for (var i=1; i<x.length; i++)
      {exp.push(normalplug(x[i],alist,blist))};
  return exp}

function newtemp () {counter++;  return 'W' + counter}

//------------------------------------------------------------------------------
// grindjs
//------------------------------------------------------------------------------

function grindjs (x)
 {if (symbolp(x)) {return x};
  if (x[0]==='quote') {return "'" + x[1] + "'"};
  if (x[0]==='dot') {return grindjs(x[1]) + '[' + x[2] + ']'};
  if (x[0]==='bind') {return "var " + x[1] + " = " + grindjs(x[2])};
  if (x[0]==='if') {return "if (" + grindjs(x[1]) + ") " + grindjs(x[2])};
  if (x[0]==='not') {return "!" + grindjs(x[1])};
  if (x[0]==='block')
     {var answer = '{' + grindjs(x[1]) + '; ';
      for (var i=2; i<x.length; i++)
          {answer += grindjs(x[i]) + '; '};
      answer += '}';
      return answer};
  if (x[0]==='loop')
     {return 'for (var ' + x[1] + ' = ' + x[2] + '; ' + x[1] + '<' + x[3] + '; ' + x[1] + '++)' + grindjs(x[4])}
  if (x[0]=== 'function')
     {var answer = 'function ' + x[1] + grindjsarglist(x[2]) + '{';
      for (var i=3; i<x.length; i++)
          {answer += grindjs(x[i]) + '; '};
      answer += '}';
      return answer}
  return x[0] + grindjsarglist(x.slice(1))}

function grindjsarglist (p)
 {var exp = '(';  if (p.length>0) {exp += grindjs(p[0])};  for (var i=1; i<p.length; i++)      {exp = exp + ',' + grindjs(p[i])}  exp += ')';  return exp}

//------------------------------------------------------------------------------// additions to interpreter//------------------------------------------------------------------------------

function baseanswers (rel)
 {var answers = seq();
  var data = indexees(rel,repository);  for (var i=1; i<arguments.length; i++)      {var dum = baseindexps(arguments[i],repository);       if (dum.length<data.length) {data = dum}};
  for (var i=0; i<data.length; i++)
      {var flag = true;
       for (var j=0; j<arguments.length; j++)
           {if (!simplematchp(arguments[j],data[i][j])) {flag = false; break}};
       if (flag) {answers.push(data[i])}};
  return answers}

function baseindexps (x,facts) {if (varp(x)) {return facts};  if (symbolp(x)) {return indexees(x,facts)};  var best = indexees(x[0],facts);  for (var i=1; i<x.length; i++)      {var dum = baseindexps(x[i],facts);       if (dum.length<best.length) {best = dum}};  return best}

function simplematchp (x,y) {if (x===y) {return true};  if (varp(x)) {return true};  if (symbolp(x)) {return false};  if (x.length!==y.length) {return false};  for (var i=0; i<x.length; i++)      {if (!simplematchp(x[i],y[i])) {return false}};  return true}

//------------------------------------------------------------------------------

function getarity (rel,rules)
 {for (var i=0; i<rules.length; i++)
      {var answer = getarityexp(rel,rules[i]);
       if (answer) {return answer}};
  return 0}

function getarityexp (rel,rule)
 {if (rule===rel) {return 0};
  if (symbolp(rule)) {return false};
  if (rule[0]===rel) {return rule.length-1};
  if (rule[0]==='not') {return getarityexp(rel,rule[1])};
  if (rule[0]==='rule')
     {for (var i=1; i<rule.length; i++)
          {var answer = getarityexp(rel,rule[i]);
          if (answer) {return answer}}
      return false};
  return false}

//------------------------------------------------------------------------------

function getbasesfromrules (rules)
 {var bases = seq();
  var views = getviews(rules);
  for (var i=0; i<rules.length; i++)
      {bases = getbasesexp(rules[i],views,bases)};
  return bases}

function getbasesexp (rule,views,bases)
 {if (symbolp(rule))
     {if (!find(rule,views)) {return adjoin(rule,bases)}};
  if (rule[0]==='is') {return bases};
  if (rule[0]==='same') {return bases};
  if (rule[0]==='distinct') {return bases};
  if (rule[0]==='provable') {return getbasesexp(rule[1],views,bases)};
  if (rule[0]==='not') {return getbasesexp(rule[1],views,bases)};
  if (rule[0]==='and' || rule[0]==='or')
     {for (var i=1; i<rule.length; i++)
          {bases = getbasesexp(rule[i],views,bases)};
      return bases};
  if (rule[0]==='rule')
     {for (var i=2; i<rule.length; i++)
          {bases = getbasesexp(rule[i],views,bases)};
      return bases};
  if (find(rule[0],views)) {return bases};
  return adjoin(rule[0],bases)}
//------------------------------------------------------------------------------// End of Script//------------------------------------------------------------------------------

