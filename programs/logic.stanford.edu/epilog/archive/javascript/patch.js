//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
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

function componebagofall (p,pl,al,cont,facts,rules)
  var ol = seq();
  var answers = seq();
  compall(p[1],p[2],seq(),al,nil,answers,facts,rules);
  var result = seq('listof').concat(answers);
  if (vnifyp(p[3],al,result,al,ol))
     {componeexit(pl,al,cont,facts,rules);
      backup(ol);
      return true};

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