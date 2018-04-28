//------------------------------------------------------------------------------
// Debugger
//------------------------------------------------------------------------------

function toggle (name)
 {node = document.getElementById(name);
  if (node.style.display=='none') {node.style.display = ''}
  else {node.style.display = 'none'}}

function tryit ()
 {var str = document.getElementById('scriptarea').value;
  document.getElementById('valuearea').innerHTML = eval(str)}

function tryit ()
 {var str = document.getElementById('scriptarea').value;
  var beg = new Date();
  var result = eval(str);
  var end = new Date();
  var total = end.getTime()-beg.getTime();
  //document.getElementById('timearea').innerHTML = message
  var message = total + ' milliseconds<br/>' + result;
  document.getElementById('valuearea').innerHTML = message}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
