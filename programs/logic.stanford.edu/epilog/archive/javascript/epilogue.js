//------------------------------------------------------------------------------

  if (x == y) {return bl};
 {for (var i=0; i<data.length; i++) {insertfact(data[i],theory)};
  return theory}

function definemorerules (theory,data)
 {for (var i=0; i<data.length; i++) {insertrule(data[i],theory)};
  return theory}

function emptytheory (theory)

function insertrule (p,theory)

function uninsert (p,theory)

// need fact and rule uninsertion
  if (!isNaN(Number(x))) {return theory};


var inferences = 0;
  inferences = inferences + 1;
  if (proonebackground(p,pl,al,facts,rules)) {return true}
  for (var i=1; i<p.length-1; i++)
      {var arg = plug(p[i],al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  return false}
  for (var i=1; i<p.length-1; i++)
      {var arg = plug(p[i],al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';
  return false}
  var arg = parseFloat(plug(p[1],al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(plug(p[i],al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';
  return false}
  var rl = viewindexps(p,rules);
  inferences = inferences + 1;
  for (var i=1; i<p.length-1; i++)
      {var arg = plug(p[i],al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  return false}
  for (var i=1; i<p.length-1; i++)
      {var arg = plug(p[i],al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';
  return false}
  var arg = parseFloat(plug(p[1],al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(plug(p[i],al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';
  return false}
  return false}
  inferences = inferences + 1;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  var ol = seq();
     {if (componeexit(pl,al,cont,facts,rules))
  return false}
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';
     {if (componeexit(pl,al,cont,facts,rules))
  return false}
  var arg = parseFloat(pluug(p[1],al,al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(pluug(p[i],al,al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';
     {if (componeexit(pl,al,cont,facts,rules))
  return false}

// backup in any case

function componebagofall (p,pl,al,cont,facts,rules)
  var ol = seq();
  var result = seq('listof').concat(compfinds(p[1],p[2],facts,rules));
  if (vnifyp(p[3],al,result,al,ol))
     {if (componeexit(pl,al,cont,facts,rules))

  inferences = inferences + 1;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  var ol = seq();
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}
  var arg = parseFloat(pluug(p[1],al,al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(pluug(p[i],al,al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compallbagofall (x,p,pl,al,cont,results,facts,rules)
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  var result = seq('listof').concat(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};


     {compallexit(x,pl,al,cont,results,facts,rules)}}
      {compall(x,p[i],pl,al,cont,results,facts,rules)}}
// residue
//   current version assumes query is ground
//   current version does not handle not
//   current version does not do consistency check
//------------------------------------------------------------------------------
  return answer}
     {return resoneexit(pl,al,adjoin(p,rl),prims,rules)};
     {return resallprim(p,pl,al,rl,prims,rules)};

function resallprim (p,pl,al,rl,prims,rules)
 {return resallexit(pl,al,rl.concat([p]),prims,rules)}
  return false}

//------------------------------------------------------------------------------
// viewresidue
//   current version assumes query is ground
//   current version does not handle not
//   current version does not do consistency check
//------------------------------------------------------------------------------
  return answer}
     {return viewresoneexit(pl,al,adjoin(p,rl),prims,facts,rules)};
     {return viewresoneexit(pl,al,rl,prims,facts,rules)};
     {return viewresoneexit(pl,al,prims,facts,rules)}
     {return viewresallexit(pl,al,adjoin(p,rl),prims,facts,rules)};
  viewresallbackground(p,pl,al,rl,prims,facts,rules);
     {viewresallexit(pl,al,rl,prims,facts,rules)}}
     {viewresallexit(pl,al,prims,facts,rules)}}
      {viewresall(p[i],pl,al,rl,prims,facts,rules)}}
              {viewresall(copy[2],concatenate(copy.slice(3),pl),bl,rl,prims,facts,rules)}}
     {return viewresall(pl[0],tail(pl),bl,rl,prims,facts,rules)};
  return false}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//rules = seq(read('r(X) :- p(X,Y) & q(Y)'), read('q(b)'));
//query = read('r(a)');

function pp (lop,rop)
     {return rop != '=' && rop != ':' && rop != '~' && rop != '&' && rop != '|' &&
             rop != '=>' && rop != '<=' && rop != '<=>'};
     {return rop != '=' && rop != ':' && rop != '~' && rop != '&' && rop != '|' &&
             rop != '=>' && rop != '<=' && rop != '<=>'};
     {return rop != '=' && rop != ':' && rop != '~' && rop != '&' && rop != '|' &&
             rop != '=>' && rop != '<=' && rop != '<=>'};
     {return rop != '=' && rop != ':' && rop != '~' && rop != '&' && rop != '|' &&
             rop != '=>' && rop != '<=' && rop != '<=>'};

function printseq (p)

//------------------------------------------------------------------------------

  if (p == null) {return ''};
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
      {if (arities[rel] == 'mixed')
          {errors[errors.length] = 'Mixed arity: ' + grind(rel)}};
  return errors}

function getarities (p,arities)
 {if (symbolp(p)) {return addarity(p,0,arities)}
  if (p[0] == 'distinct') {return arities};
  if (p[0] == 'not') {return getarities(p[1],arities)};
  if (p[0] == 'and' || p[0] == 'rule')
     {for (var i=1; i<p.length; i++)
          {arities = getarities(p[i],arities)};
      return arities};
  return addarity(p[0],p.length-1,arities)}

function addarity (x,n,arities)
 {if (arities[x] == null) {arities[x] = n; return arities};
  if (arities[x] == n) {return arities};
  arities[x] = 'mixed';
  return arities}

//------------------------------------------------------------------------------

function findsafetyerrors (data)
 {var errors = seq();
  for (var i=0; i<data.length; i++)
      {if (!safep(data[i]))
          {errors[errors.length] = 'Unsafe rule: ' + grind(data[i])}};
  return errors}

function safep (exp)
  for (var i=2; i<rule.length; i++) {hs = headvars(rule[i],hs)};
     {for (var i=0; i<exp.length; i++) {hs = headvars(exp[i],hs)}};
  return(hs)}

function bodyvars (exp,vs)
  return(vs)}

function groundedp (exp,vs)
 {if (symbolp(p)) {return p};
  if (p[0]=='not' || p[0]=='rule') {return operator(p[1])};
  return p[0]}

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

function checkstratifiednegation (datum,strata)
 {if (symbolp(datum)) {return true};
  if (datum[0]!='rule') {return true};
  var stratum = strata[operator(datum[1])];
  for (var j=2; j<datum.length; j++)
      {if (!symbolp(datum[j]) && datum[j][0]=='not')
          {var op = operator(datum[j][1]);
           if (strata[op]>=stratum) {return false}}};
  return true}

function checkstratifiedrecursion (datum,strata)
 {if (symbolp(datum)) {return true};
  if (datum[0]!='rule') {return true};
  var stratum = strata[operator(datum[1])];
  var hs = vars(datum[1]);
  var vs = seq();
      {if (symbolp(datum[j]) || datum[j][0]!='not')
          {if (strata[operator(datum[j])]>=stratum)
              {hs = varsexp(datum[j],hs)}
           else {vs = varsexp(datum[j],vs)}}};
  for (var i=0; i<hs.length; i++)
 {var relations = getrelations(data);
  var strata = seq();
  for (var i=0; i<relations.length; i++)
      {computestratum(relations[i],strata,data,seq())}
  return strata}

function getrelations (data)
 {var relations = seq();
  for (var i=0; i<data.length; i++)
      {relations = getrelationsfromrule(data[i],relations)};
  return relations}

function getrelationsfromrule (rule,relations)
 {if (symbolp(rule)) {return adjoin(rule,relations)};
  for (var j=1; j<rule.length; j++)
      {var op = operator(rule[j]);
       if (op!='distinct') {relations = adjoin(op,relations)}};
  return relations}

function computestratum (r,strata,data,history)
 {if (strata[r] != null) {return strata[r]};
  var stratum = 0;
  strata[r] = stratum;
  for (var i=0; i<data.length; i++)
      {stratum = Math.max(stratum,computestratumfromrule(r,strata,data[i],data,history))};
  strata[r] = stratum;
  return stratum}

function computestratumfromrule (r,strata,rule,data,history)
 {if (symbolp(rule)) {return 0};
  if (rule[0]!='rule') {return 0};
  if (operator(rule[1])!=r) {return 0};
  stratum = 0;
  for (var j=2; j<rule.length; j++)
      {var op=operator(rule[j]);
       stratum = Math.max(stratum,computestratum(op,strata,data,history)+1)};
  return stratum}

//------------------------------------------------------------------------------