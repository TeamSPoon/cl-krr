//------------------------------------------------------------------------------// patch to epilog to add bagofall
//------------------------------------------------------------------------------//------------------------------------------------------------------------------// compfindp// compfindx// compfinds//------------------------------------------------------------------------------function compfindp (query,facts,rules) {return compfindx('true',query,facts,rules)}function compfindx (result,query,facts,rules) {thing = result;  answer = false;  if (compone(query,seq(),seq(),nil,facts,rules)) {return answer};  return false}function compfinds (result,query,facts,rules) {var results = seq();  compall(result,query,seq(),seq(),nil,results,facts,rules);  return uniquify(results)}//------------------------------------------------------------------------------function compone (p,pl,al,cont,facts,rules) {if (epitrace) {console.log(grind(pluug(p,al,al)) + '-' + pl + '-' + al)};
  inferences = inferences + 1;  if (symbolp(p)) {return componeatom(p,pl,al,cont,facts,rules)}  if (p[0] == 'same') {return componesame(p,pl,al,cont,facts,rules)}  if (p[0] == 'distinct') {return componedistinct(p,pl,al,cont,facts,rules)}  if (p[0] == 'matches') {return componematches(p,pl,al,cont,facts,rules)}  if (p[0] == 'plus') {return componeplus(p,pl,al,cont,facts,rules)}  if (p[0] == 'times') {return componetimes(p,pl,al,cont,facts,rules)}  if (p[0] == 'min') {return componemin(p,pl,al,cont,facts,rules)}  if (p[0] == 'bagofall') {return componebagofall(p,pl,al,cont,facts,rules)}  if (p[0] == 'not') {return componenot(p,pl,al,cont,facts,rules)}  if (p[0] == 'and') {return componeand(p,pl,al,cont,facts,rules)}  if (p[0] == 'or') {return componeor(p,pl,al,cont,facts,rules)}  if (componebackground(p,pl,al,cont,facts,rules)) {return true};  return componers(p,pl,al,cont,facts,rules)}function componeatom (p,pl,al,cont,facts,rules) {if (p == 'true') {return componeexit(pl,al,cont,facts,rules)};  if (p == 'false') {return false};  return componers(p,pl,al,cont,facts,rules)}function componesame (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {return componeexit(pl,al,cont,facts,rules)};  backup(ol);  return false}function componedistinct (p,pl,al,cont,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return componeexit(pl,al,cont,facts,rules)}function componematches (p,pl,al,cont,facts,rules) {if (symbolp(p[1]))     {var matches = p[1].match(p[2]);      for (var i=0; i<matches.length; i++)          {var ol = seq();           if (vnifyp(p[3],al,matches[i],al,ol))              {if (componeexit(pl,bl,cont,facts,rules))                  {backup(ol); return true}}}};  return false}function componeplus (p,pl,al,cont,facts,rules) {var total=0;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  var ol = seq();  if (vnify(p[p.length-1],al,total,al,ol))
     {if (componeexit(pl,al,cont,facts,rules))         {backup(ol); return true}};
  return false}function componetimes (p,pl,al,cont,facts,rules) {var result=1;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';  var ol = seq();  if (vnify(p[p.length-1],al,result,al,ol))
     {if (componeexit(pl,al,cont,facts,rules))         {backup(ol); return true}};
  return false}function componemin (p,pl,al,cont,facts,rules) {var min = 0;
  var arg = parseFloat(pluug(p[1],al,al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(pluug(p[i],al,al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';  var ol = seq();  if (vnify(p[p.length-1],al,min,al,ol))
     {if (componeexit(pl,al,cont,facts,rules))         {backup(ol); return true}};
  return false}

function componebagofall (p,pl,al,cont,facts,rules) {p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  var result = seq('listof').concat(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {componeexit(pl,al,cont,facts,rules);
      backup(ol);
      return true};  return false}
function componenot (p,pl,al,cont,facts,rules) {if (!compone(p[1],seq(),al,nil,facts,rules))     {return componeexit(pl,al,cont,facts,rules)};  return false}function componeand (p,pl,al,cont,facts,rules) {return componeexit(concatenate(tail(p),pl),al,cont,facts,rules)}function componeor (p,pl,al,cont,facts,rules) {for (var i=0; i<p.length; i++)      {if (compone(p[i],pl,al,cont,facts,rules)) {return true}};  return false}function componebackground (p,pl,al,cont,facts,rules) {var data = facts;  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {if (componeexit(pl,al,cont,facts,rules)) {backup(ol); return true};           backup(ol)}}  return false}function componers (p,pl,al,cont,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0] == 'rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               if (compone(data[i][2],ql,bl,nc,facts,rules))                  {backup(ol); return true};               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {if (componeexit(pl,al,cont,facts,rules))                    {backup(ol); return true};                 backup(ol)}}}  return false}function componeexit (pl,al,cont,facts,rules) {if (pl.length != 0) {return compone(pl[0],tail(pl),al,cont,facts,rules)};  if (nullp(cont)) {answer = pluug(thing,al,al); return true};  return componeexit(car(cont)[0],car(cont)[1],car(cont)[2],facts,rules)}//------------------------------------------------------------------------------function compall (x,p,pl,al,cont,results,facts,rules) {if (epitrace) {console.log(grind(pluug(p,al,al)) + '-' + pl + '-' + al)};
  inferences = inferences + 1;  if (symbolp(p)) {return compallatom(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'same') {return compallsame(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'distinct') {return compalldistinct(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'matches') {return compallmatches(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'plus') {return compallplus(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'times') {return compalltimes(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'min') {return compallmin(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'bagofall') {return compallbagofall(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'not') {return compallnot(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'and') {return compalland(x,p,pl,al,cont,results,facts,rules)}  if (p[0] == 'or') {return compallor(x,p,pl,al,cont,results,facts,rules)}  compallbackground(x,p,pl,al,cont,results,facts,rules);  return compallrs(x,p,pl,al,cont,results,facts,rules)}function compallatom (x,p,pl,al,cont,results,facts,rules) {if (p == 'true') {return compallexit(x,pl,al,cont,results,facts,rules)};  if (p == 'false') {return false};  return compallrs(x,p,pl,al,cont,results,facts,rules)}function compallsame (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol))     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)}}function compalldistinct (x,p,pl,al,cont,results,facts,rules) {var ol = seq();  if (vnifyp(p[1],al,p[2],al,ol)) {backup(ol); return false};  return compallexit(x,pl,al,cont,results,facts,rules)}function compallmatches (x,p,pl,al,cont,results,facts,rules) {if (symbolp(p[1]))     {var matches = p[1].match(p[2]);      for (var i=0; i<matches.length; i++)          {var ol = seq();           if (vnifyp(p[3],al,matches[i],al,ol))              {compallexit(x,pl,bl,cont,results,facts,rules); backup(ol)}}}  return false}function compallplus (x,p,pl,al,cont,results,facts,rules) {var total=0;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg=='true') {total += 1}
          else {arg=parseFloat(arg);
                if (!isNaN(arg)) {total += arg}}};
  total = total+'';
  var ol = seq();  if (vnify(p[p.length-1],al,total,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}function compalltimes (x,p,pl,al,cont,results,facts,rules) {var result=1;
  for (var i=1; i<p.length-1; i++)
      {var arg = pluug(p[i],al,al);
       if (arg!='true')
          {arg=parseFloat(arg);
           if (isNaN(arg)) {result = 0}
              else {result = result * arg}}};
  result = result + '';  var ol = seq();  if (vnify(p[p.length-1],al,result,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}function compallmin (x,p,pl,al,cont,results,facts,rules) {var min = 0;
  var arg = parseFloat(pluug(p[1],al,al));
  if (!isNaN(arg)) {min = arg};
  for (var i=2; i<p.length-1; i++)
      {var arg = parseFloat(pluug(p[i],al,al));
       if (isNaN(arg)) {arg = 0};
       if (arg<min) {min = arg}};
  min = min+'';  var ol = seq();  if (vnify(p[p.length-1],al,min,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules); backup(ol)};
  return false}

function compallbagofall (x,p,pl,al,cont,results,facts,rules) {p = pluug(p,al,al);
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  var result = seq('listof').concat(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {compallexit(x,pl,al,cont,results,facts,rules);
      backup(ol);
      return false};  return false}

function compallnot (x,p,pl,al,cont,results,facts,rules) {if (compone(p[1],seq(),al,nil,facts,rules) == false)
     {compallexit(x,pl,al,cont,results,facts,rules)}}function compalland (x,p,pl,al,cont,results,facts,rules) {compallexit(x,concatenate(tail(p),pl),al,cont,results,facts,rules)}function compallor (x,p,pl,al,cont,results,facts,rules) {for (var i=0; i<p.length; i++)
      {compall(x,p[i],pl,al,cont,results,facts,rules)}}function compallbackground (x,p,pl,al,cont,results,facts,rules) {var data = facts;  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (vnifyp(data[i],bl,p,al,ol))          {compallexit(x,pl,al,cont,results,facts,rules);           backup(ol)}}}function compallrs (x,p,pl,al,cont,results,facts,rules) {var data = viewindexps(p,rules);  for (var i=0; i<data.length; i++)      {var bl = seq();       var ol = seq();       if (data[i][0] == 'rule')          {if (vnifyp(data[i][1],bl,p,al,ol))              {var ql = data[i].slice(3);               var nc = cons(seq(pl,al,cont),cont);               compall(x,data[i][2],ql,bl,nc,results,facts,rules);               backup(ol)}}       else {if (vnifyp(data[i],bl,p,al,ol))                {compallexit(x,pl,al,cont,results,facts,rules);                 backup(ol)}}}}function compallexit (x,pl,al,cont,results,facts,rules) {if (pl.length != 0) {return compall(x,pl[0],tail(pl),al,cont,results,facts,rules)};  if (nullp(cont)) {results.push(pluug(x,al,al)); return true};  return compallexit(x,car(cont)[0],car(cont)[1],car(cont)[2],results,facts,rules)}//------------------------------------------------------------------------------//------------------------------------------------------------------------------//------------------------------------------------------------------------------