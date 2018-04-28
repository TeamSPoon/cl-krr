// Morten's JavaScript Tree Menu
// written by Morten Wang <morten@treemenu.com> (c) 1998-2000
// This is version 2.2.6, dated 2000-03-30

// The script is freely distributable
// It may be used (and modified) as you wish, but retain this message
// For more information about the menu visit its home page
// http://www.treemenu.com/

/******************************************************************************
* Define the MenuItem object.                                                 *
******************************************************************************/
function MTMenuItem(text, url, target, color, desc, icon) {
  this.text = text;
  this.url = url ? url : "";
  this.target =  target ? target : "";
  this.icon = icon ? icon : "";
  this.color = color ? color : "red";
  this.desc = desc ? desc : "No definition at this time";

  this.number = MTMSubNumber++;

  this.submenu     = null;
  this.expanded    = false;
  this.MTMakeSubmenu = MTMakeSubmenu;
}

function MTMakeSubmenu(menu) {
  this.submenu = menu;
}

/******************************************************************************
* Define the Menu object.                                                     *
******************************************************************************/

function MTMenu() {
  this.items   = new Array();
  this.MTMAddItem = MTMAddItem;
}

function MTMAddItem(item) {
  this.items[this.items.length] = item;
}

/******************************************************************************
* Define the icon list, addIcon function and MTMIcon item.                    *
******************************************************************************/

function IconList() {
  this.items = new Array();
  this.addIcon = addIcon;
}

function addIcon(item) {
  this.items[this.items.length] = item;
}

function MTMIcon(iconfile, match, type) {
  this.file = iconfile;
  this.match = match;
  this.type = type;
}

/******************************************************************************
* Global variables.  Not to be altered unless you know what you're doing.     *
* User-configurable options are at the end of this document.                  *
******************************************************************************/

var MTMLoaded = false;
var MTMLevel;
var MTMBar = new Array();
var MTMIndices = new Array();
var MTMBrowser = null;
var MTMNN3 = false;
var MTMNN4 = false;
var MTMIE4 = false;
var MTMUseStyle = true;


if(navigator.appName == "Netscape" && navigator.userAgent.indexOf("WebTV") == -1) {
  if(parseInt(navigator.appVersion) == 3 && (navigator.userAgent.indexOf("Opera") == -1)) {
    MTMBrowser = true;
    MTMNN3 = true;
    MTMUseStyle = false;
  } else if(parseInt(navigator.appVersion) >= 4) {
    MTMBrowser = true;
    MTMNN4 = true;
  }
} else if (navigator.appName == "Microsoft Internet Explorer" && parseInt(navigator.appVersion) >= 4) {
  MTMBrowser = true;
  MTMIE4 = true;
}

var MTMClickedItem = false;
var MTMExpansion = false;

var MTMSubNumber = 1;
var MTMTrackedItem = false;
var MTMTrack = false;

var MTMPreHREF = "";
if(MTMIE4 || MTMNN3) {
  MTMPreHREF += document.location.href.substring(0, document.location.href.lastIndexOf("/") +1);
}

var MTMFirstRun = true;
var MTMCurrentTime = 0; // for checking timeout.
var MTMUpdating = false;
var MTMWinSize, MTMyval;
var MTMOutputString = "";

/******************************************************************************
* Code that picks up frame names of frames in the parent frameset.            *
******************************************************************************/

if(MTMBrowser) {
  var MTMFrameNames = new Array();
  for(i = 0; i < parent.frames.length; i++)
    MTMFrameNames[i] = parent.frames[i].name;
}

/******************************************************************************
* Dummy function for sub-menus without URLs                                   *
* Thanks to Michel Plungjan for the advice. :)                                *
******************************************************************************/

function myVoid() { ; }

/******************************************************************************
* Functions to draw the menu.                                                 *
******************************************************************************/

function MTMSubAction(SubItem, ReturnValue) {

  SubItem.expanded = (SubItem.expanded) ? false : true;
  if(SubItem.expanded) {
    MTMExpansion = true;
  }

  MTMClickedItem = SubItem.number;

  if(MTMTrackedItem && MTMTrackedItem != SubItem.number) {
    MTMTrackedItem = false;
  }

  if(!ReturnValue) {
    setTimeout("MTMDisplayMenu()", 10);
  }

  return ReturnValue;
}

function MTMStartMenu() {

  MTMLoaded = true;
  if(MTMFirstRun) {
    MTMCurrentTime++;
    if(MTMCurrentTime == MTMTimeOut) { // call MTMDisplayMenu
      setTimeout("MTMDisplayMenu()",10);
    } else {
      setTimeout("MTMStartMenu()",100);
    }
  } 
}

function MTMDisplayMenu() {
  if(MTMBrowser && !MTMUpdating) {
    MTMUpdating = true;
    MTMFirstRun = false;

    if(MTMTrack) { MTMTrackedItem = MTMTrackExpand(menu); }

    if(MTMExpansion && MTMSubsAutoClose) { MTMCloseSubs(menu); }

    MTMLevel = 0;
    MTMDoc = parent.frames[MTMenuFrame].document; //added ;
    MTMDoc.open("text/html", "replace");
    MTMOutputString = '<html><head>';
    if(MTMLinkedSS) {
      MTMOutputString += '<link rel="stylesheet" type="text/css" href="' + MTMPreHREF + MTMSSHREF + '">';
    } else if(MTMUseStyle) {
      MTMOutputString += '<style type="text/css">body {color:' + MTMTextColor + ';background:';
      MTMOutputString += (MTMBackground == "") ? MTMBGColor : MTMakeBackImage(MTMBackground);
      MTMOutputString += ';} #root {color:' + MTMRootColor + ';background:' + ((MTMBackground == "") ? MTMBGColor : 'transparent') + ';font-family:' + MTMRootFont + ';font-size:' + MTMRootCSSize + ';} ';
      MTMOutputString += 'a {font-family:' + MTMenuFont + ';font-size:' + MTMenuCSSize + ';text-decoration:none;color:' + MTMLinkColor + ';background:' + MTMakeBackground() + ';} ';
      MTMOutputString += MTMakeA('pseudo', 'hover', MTMAhoverColor);
      MTMOutputString += MTMakeA('class', 'tracked', MTMTrackColor);
      MTMOutputString += MTMakeA('class', 'subexpanded', MTMSubExpandColor);
      MTMOutputString += MTMakeA('class', 'subclosed', MTMSubClosedColor) + '</style>';
    }

    MTMOutputString += '</head><body ';
    if(MTMBackground != "") {
      MTMOutputString += 'background="' + MTMPreHREF + MTMenuImageDirectory + MTMBackground + '" ';
    }
    MTMOutputString += 'bgcolor="' + MTMBGColor + '" text="' + MTMTextColor + '" link="' + MTMLinkColor + '" vlink="' + MTMLinkColor + '" alink="' + MTMLinkColor + '">';
    MTMOutputString += '<table border="0" cellpadding="0" cellspacing="0" width="' + MTMTableWidth + '">';
    MTMOutputString += '<tr valign="top"><td nowrap><img src="' + MTMPreHREF + MTMenuImageDirectory
+ MTMRootIcon + '" align="left" border="0" vspace="0" hspace="0" height="18" width="18">';
    if(MTMUseStyle) {
      MTMOutputString += '<span id="root">&nbsp;' + MTMenuText + '</span>';
    } else {
      MTMOutputString += '<font size="' + MTMRootFontSize + '" face="' + MTMRootFont + '" color="' + MTMRootColor + '">' + MTMenuText + '</font>';
    }
    MTMDoc.writeln(MTMOutputString + '</td></tr>');

    MTMListItems(menu);

    MTMDoc.writeln('</table></body></html>');
    MTMDoc.close();

    if((MTMClickedItem || MTMTrackedItem) && (MTMNN4 || MTMIE4) && !MTMFirstRun) {
      MTMItemName = "sub" + (MTMClickedItem ? MTMClickedItem : MTMTrackedItem);
      if(document.layers && parent.frames[MTMenuFrame].scrollbars) {    
        MTMyval = parent.frames[MTMenuFrame].document.anchors[MTMItemName].y;
        MTMWinSize = parent.frames[MTMenuFrame].innerHeight;
      } else {
        if (MTMIE4) {
	  MTMyval = MTMGetPos(parent.frames[MTMenuFrame].document.all[MTMItemName]); // ??
	}
        MTMWinSize = parent.frames[MTMenuFrame].document.body.offsetHeight;
      }
      if(MTMyval > (MTMWinSize - 60)) {
        parent.frames[MTMenuFrame].scrollBy(0, parseInt(MTMyval - (MTMWinSize * 1/3)));
      }
    }

    MTMClickedItem = false;
    MTMExpansion = false;
    MTMTrack = false;
  }
MTMUpdating = false;
}

function MTMListItems(menu) {
  var i, isLast;
  for (i = 0; i < menu.items.length; i++) {
    MTMIndices[MTMLevel] = i;
    isLast = (i == menu.items.length -1);
    MTMDisplayItem(menu.items[i], isLast);

    if (menu.items[i].submenu && menu.items[i].expanded) {
      MTMBar[MTMLevel] = (isLast) ? false : true;
      MTMLevel++;
      MTMListItems(menu.items[i].submenu);
      MTMLevel--;
    } else {
      MTMBar[MTMLevel] = false;
    } 
  }
}

function MTMDisplayItem(item, last) {
  var i, img, more;

  if(item.submenu) {
    var MTMouseOverText;

    var MTMClickCmd;
    var MTMDblClickCmd = false;
    var MTMfrm = "parent.frames['code']";
    var MTMref = '.menu.items[' + MTMIndices[0] + ']';

    if(MTMLevel > 0) {
      for(i = 1; i <= MTMLevel; i++) {
        MTMref += ".submenu.items[" + MTMIndices[i] + "]";
      }
    }

    if(!MTMEmulateWE && !item.expanded && (item.url != "")) {
      MTMClickCmd = "return " + MTMfrm + ".MTMSubAction(" + MTMfrm + MTMref + ",true);";
    } else {
      MTMClickCmd = "return " + MTMfrm + ".MTMSubAction(" + MTMfrm + MTMref + ",false);";
    }

    if(item.url == "") {
      MTMouseOverText = (item.text.indexOf("'") != -1) ? MTMEscapeQuotes(item.text) : item.text;
    } else {
      MTMouseOverText = "Expand/Collapse";
    }
  }

  MTMOutputString = '<tr valign="top"><td nowrap>';
  if(MTMLevel > 0) {
    for (i = 0; i < MTMLevel; i++) {
      MTMOutputString += (MTMBar[i]) ? MTMakeImage("menu_bar.gif") : MTMakeImage("menu_pixel.gif");
    }
  }

  more = false;
  if(item.submenu) {
    if(MTMSubsGetPlus || MTMEmulateWE) {
      more = true;
    } else {
      for (i = 0; i < item.submenu.items.length; i++) {
        if (item.submenu.items[i].submenu) {
          more = true;
        }
      }
    }
  }
  if(!more) {
    img = (last) ? "menu_corner.gif" : "menu_tee.gif";
  } else {
    if(item.expanded) {
      img = (last) ? "menu_corner_minus.gif" : "menu_tee_minus.gif";
    } else {
      img = (last) ? "menu_corner_plus.gif" : "menu_tee_plus.gif";
    }
    if(item.url == "" || item.expanded || MTMEmulateWE) {
      MTMOutputString += MTMakeVoid(item, MTMClickCmd, MTMouseOverText);
    } else {
      MTMOutputString += MTMakeLink(item, true)  + ' onclick="' + MTMClickCmd + '">';
    }
  }
  MTMOutputString += MTMakeImage(img);

  if(item.submenu) {
    if(MTMEmulateWE && item.url != "") {
      MTMOutputString += '</a>' + MTMakeLink(item, false) + '>';
    }

    img = (item.expanded) ? "menu_folder_open.gif" : "menu_folder_closed.gif";

    if(!more) {
      if(item.url == "" || item.expanded) {
        MTMOutputString += MTMakeVoid(item, MTMClickCmd, MTMouseOverText);
      } else {
        MTMOutputString += MTMakeLink(item, true) + ' onclick="' + MTMClickCmd + '">';
      }
    }
    MTMOutputString += MTMakeImage(img);

  } else {
    MTMOutputString += MTMakeLink(item, true) + '>';
    img = (item.icon != "") ? item.icon : MTMFetchIcon(item.url);
    MTMOutputString += MTMakeImage(img);
  }

  if(item.submenu && (item.url != "") && (item.expanded && !MTMEmulateWE)) {
    MTMOutputString += '</a>' + MTMakeLink(item, false) + '>';
  }

  if(MTMNN3 && !MTMLinkedSS) {
    var stringColor;
    if(item.submenu && (item.url == "") && (item.number == MTMClickedItem)) {
      stringColor = (item.expanded) ? MTMSubExpandColor : MTMSubClosedColor;
    } else if(MTMTrackedItem && MTMTrackedItem == item.number) {
      stringColor = MTMTrackColor;
    } else {
      stringColor = MTMLinkColor;
    }
    //alert(item.text);
    if (SelectedName == item.text) {
      MTMOutputString += '<font weight=500 color="' + stringColor + '" size="' + MTMenuFontSize + '" face="' + MTMenuFont + '">';
    } else {
      MTMOutputString += '<font color="' + stringColor + '" size="' + MTMenuFontSize + '" face="' + MTMenuFont + '">';
    }
  }
  else {
// HERE TOO
    if (SelectedName == item.text) {
       MTMOutputString += '<font weight=500 color="' + item.color + '" size="' + MTMenuFontSize + '" face="' + MTMenuFont + '">';
    } else {
       MTMOutputString += '<font color="' + item.color + '" size="' + MTMenuFontSize + '" face="' + MTMenuFont + '">';
    }
  }

  MTMOutputString += '&nbsp;' + item.text + ((MTMNN3 && !MTMLinkedSS) ? '</font>' : '') + '</a>' ;
  MTMDoc.writeln(MTMOutputString + '</td></tr>');
}

function MTMEscapeQuotes(myString) {
  var newString = "";
  var cur_pos = myString.indexOf("'");
  var prev_pos = 0;
  while (cur_pos != -1) {
    if(cur_pos == 0) {
      newString += "\\";
    } else if(myString.charAt(cur_pos-1) != "\\") {
      newString += myString.substring(prev_pos, cur_pos) + "\\";
    } else if(myString.charAt(cur_pos-1) == "\\") {
      newString += myString.substring(prev_pos, cur_pos);
    }
    prev_pos = cur_pos++;
    cur_pos = myString.indexOf("'", cur_pos);
  }
  return(newString + myString.substring(prev_pos, myString.length));
}

function MTMTrackExpand(thisMenu) {
  var i, targetPath;
  var foundNumber = false;
  for(i = 0; i < thisMenu.items.length; i++) {
    if(thisMenu.items[i].url != "" && MTMTrackTarget(thisMenu.items[i].target)) {
      targetPath = parent.frames[thisMenu.items[i].target].location.pathname;
      if(targetPath.lastIndexOf(thisMenu.items[i].url) != -1 && (targetPath.lastIndexOf(thisMenu.items[i].url) + thisMenu.items[i].url.length) == targetPath.length) {
        return(thisMenu.items[i].number);
      }
    }
    if(thisMenu.items[i].submenu) {
      foundNumber = MTMTrackExpand(thisMenu.items[i].submenu);
      if(foundNumber) {
        if(!thisMenu.items[i].expanded) {
          thisMenu.items[i].expanded = true;
          if(!MTMClickedItem) { MTMClickedItem = thisMenu.items[i].number; }
          MTMExpansion = true;
        }
        return(foundNumber);
      }
    }
  }
return(foundNumber);
}

function MTMCloseSubs(thisMenu) {
  var i, j;
  var foundMatch = false;
  for(i = 0; i < thisMenu.items.length; i++) {
    if(thisMenu.items[i].submenu && thisMenu.items[i].expanded) {
      if(thisMenu.items[i].number == MTMClickedItem) {
        foundMatch = true;
        for(j = 0; j < thisMenu.items[i].submenu.items.length; j++) {
          if(thisMenu.items[i].submenu.items[j].expanded) {
            thisMenu.items[i].submenu.items[j].expanded = false;
          }
        }
      } else {
        if(foundMatch) {
          thisMenu.items[i].expanded = false; 
        } else {
          foundMatch = MTMCloseSubs(thisMenu.items[i].submenu);
          if(!foundMatch) {
            thisMenu.items[i].expanded = false;
          }
        }
      }
    }
  }
return(foundMatch);
}

function MTMFetchIcon(testString) {
  var i;
  for(i = 0; i < MTMIconList.items.length; i++) {
    if((MTMIconList.items[i].type == 'any') && (testString.indexOf(MTMIconList.items[i].match) != -1)) {
      return(MTMIconList.items[i].file);
    } else if((MTMIconList.items[i].type == 'pre') && (testString.indexOf(MTMIconList.items[i].match) == 0)) {
      return(MTMIconList.items[i].file);
    } else if((MTMIconList.items[i].type == 'post') && (testString.indexOf(MTMIconList.items[i].match) != -1)) {
      if((testString.lastIndexOf(MTMIconList.items[i].match) + MTMIconList.items[i].match.length) == testString.length) {
        return(MTMIconList.items[i].file);
      }
    }
  }
return("menu_link_default.gif");
}

function MTMGetPos(myObj) {
  return(myObj.offsetTop + ((myObj.offsetParent) ? MTMGetPos(myObj.offsetParent) : 0));
}

function MTMCheckURL(myURL) {
  var tempString = "";
  if((myURL.indexOf("http://") == 0) || (myURL.indexOf("https://") == 0) || (myURL.indexOf("mailto:") == 0) || (myURL.indexOf("ftp://") == 0) || (myURL.indexOf("telnet:") == 0) || (myURL.indexOf("news:") == 0) || (myURL.indexOf("gopher:") == 0) || (myURL.indexOf("nntp:") == 0) || (myURL.indexOf("javascript:") == 0)) {
    tempString += myURL;
  } else {
    tempString += MTMPreHREF + myURL;
  }
return(tempString);
}

function MTMakeVoid(thisItem, thisCmd, thisText) {
  var tempString = "";
  tempString +=  '<a name="sub' + thisItem.number + '" href="javascript:parent.frames[\'code\'].myVoid();" onclick="' + thisCmd + '" onmouseover="window.status=\'' + thisText + '\';return true;" onmouseout="window.status=\'' + window.defaultStatus + '\';return true;"';
  if(thisItem.number == MTMClickedItem) {
    var tempClass;
    tempClass = thisItem.expanded ? "subexpanded" : "subclosed";
    tempString += ' class="' + tempClass + '"';
  }
  return(tempString + '>');
}

function MTMakeLink(thisItem, addName) {
  var tempString = '<a';

  if(MTMTrackedItem && MTMTrackedItem == thisItem.number) {
    tempString += ' class="tracked"'
  }
  if(addName) {
    tempString += ' name="sub' + thisItem.number + '"';
  }
  tempString += ' href="' + MTMCheckURL(thisItem.url) + '"';
  tempString += ' title= \"' + thisItem.desc + '\"' ;
  if(thisItem.target != "") {
    tempString += ' target="' + thisItem.target + '"';
  }
 tempString +=  ' " onmouseover="window.status=\'' + thisItem.desc  + '\';  return true;" onmouseout="window.status=\'' + window.defaultStatus + '\';return true;"';
return tempString;

return tempString;
}

function MTMakeImage(thisImage) {
  return('<img src="' + MTMPreHREF + MTMenuImageDirectory + thisImage +
'" align="left" border="0" vspace="0" hspace="0" width="18" height="18">');
}

function MTMakeBackImage(thisImage) {
  var tempString = 'transparent url("' + ((MTMPreHREF == "") ? "" : MTMPreHREF);
  tempString += MTMenuImageDirectory + thisImage + '")'
  return(tempString);
}

function MTMakeA(thisType, thisText, thisColor) {
  var tempString = "";
  tempString += 'a' + ((thisType == "pseudo") ? ':' : '.');
  return(tempString + thisText + '{color:' + thisColor + ';background:' + MTMakeBackground() + ';}');
}

function MTMakeBackground() {
  return((MTMBackground == "") ? MTMBGColor : 'transparent');
}

function MTMTrackTarget(thisTarget) {
  if(thisTarget.charAt(0) == "_") {
    return false;
  } else {
    for(i = 0; i < MTMFrameNames.length; i++) {
      if(thisTarget == MTMFrameNames[i]) {
        return true;
      }
    }
  }
  return false;
}

////////////////////////////////////////////////////////////////////////
// BEGIN MFKB TREE CODE 

// the name of the component currently displayed in "text" frame
var SelectedName = "";

var url_tree = "http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/";
var url_gpd = url_tree+"components/specs/gpd/";

var build = parent.heading.document.myform.treeselect.options[parent.heading.document.myform.treeselect.selectedIndex].value;

// modify to display proper docname
var undefined;

// modify document.domain to be able to use new search
//parent.document.domain = "cs.utexas.edu";

// vars for recently visited
var max_visited = 20;
var recentlyVisited = new Array();
var visitedNo = 0;

function dispatch(name) {
  // store the current selection
  update_visited(name);
  //alert("select"+name);
  SelectedName = name;
  SelectedSpecName = spec_path["_"+SelectedName];
  if (parent.heading.document.myform.display[0].checked) { showSpec(); }
  else { showKM(); }
}


function showSpec() {
  // there is something to display
  if (spec_path["_"+SelectedName] != "NIL") {
    parent.text.document.location = url_tree+"getspec.cgi?"+spec_path["_"+SelectedName];}}

function showSystemSpec() {
  //alert("<"+url_tree+"getspec.cgi?"+SelectedSpecName+">");
  // there is something to display
  if ((SelectedName != "") && (spec_path["_"+SelectedName] != "NIL")) {
        parent.text.document.location = url_tree+"getspec.cgi?"+spec_path["_"+SelectedName]+".doc";}}

function showKM() {
  if ((SelectedName != "") && (km_path["_"+SelectedName])) {   
    //goto_km(SelectedName);
    parent.text.document.location = url_tree+km_path["_"+SelectedName];}}

function to_rkf() {
  parent.document.location="http://www.cs.utexas.edu/users/mfkb";}

function to_license(){
  parent.document.location="http://www.cs.utexas.edu/users/mfkb/manuals/simplifiedBSD.txt";}

function to_download(){
parent.document.location="http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/download-clib.html";}

// opens up a new window and displays slot dictionary
var slotdir_win = null;
function to_slot_dir(){
  if (!slotdir_win || slotdir_win.closed) {
    slotdir_win = window.open("http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/components/specs/slotdictionary.html","SlotDictionary",
            		     "width=800,height=600,status=yes,resizable=yes,scrollbars=yes");} 
  else { slotdir_win.focus();}}

function to_comp_index() {
parent.document.location="http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/compindex.html";}

function to_specs(){
  parent.document.location="http://www.cs.utexas.edu/users/kbarker/working_notes/specformat.html";}

function to_porter() {
  parent.document.location="http://www.cs.utexas.edu/users/porter/";}

function to_km(){
  parent.document.location="http://www.cs.utexas.edu/users/mfkb/RKF/km.html";}

var search_win = null;

function search () {
  //alert(parent.heading.document.myform.treeselect.options[parent.heading.document.myform.treeselect.selectedIndex].value);
  if (!search_win || search_win.closed) {
    search_win = window.open("http://www.cs.utexas.edu/users/mfkb/RKF/trunktree/proxy-search.cgi?build="+build, "search", "width=600,height=600,status=yes,resizable=yes,scrollbars=yes");
    //search_win = window.open("http://giardia.cs.utexas.edu:9000/clib-search?build="+build, "search", "width=600,height=600,status=yes,resizable=yes,scrollbars=yes");
  } else { search_win.focus();}}

function select_and_showKM(km_file) {
    SelectedName = km_file;
    SelectedSpecName = km_file;
    parent.heading.document.myform.display[1].checked = true;
    showKM();  
    window.focus(top);}

// Vinay's functions
// added to allow compatibility with kml.sed that generates calls to these
// functions instead of static urls.
// DT - May 14, 2001

function goToSpec(spec_file) {
   SelectedSpecName = spec_file;
   SelectedName = spec_file;           // TEST
   parent.heading.document.myform.display[0].checked = true;
   showSpec();
   window.focus();
}

function goToSystemSpec (spec_file) {
   SelectedName = spec_file;
   SelectedSpecName = spec_file;        // TEST
   parent.heading.document.myform.display[0].checked = true;
   showSystemSpec();  
   window.focus();
}

var gpd_win = null;
function goToGPD (gpd_file) {
  if (!gpd_win || gpd_win.closed) {
    gpd_win = window.open(url_gpd+gpd_file,"GPD",
            		     "width=800,height=600,status=yes,resizable=yes,scrollbars=yes");
  } else { gpd.document.location(url_gpd+gpd_file);
	   gpd_win.focus();}
}

// old function, replaced by goToSpec
//function select_and_showSpec (spec_file){
//   alert(spec_file);
//   SelectedName = spec_file;
//   parent.heading.document.myform.display[0].checked = true;
//   showSpec();  
//   window.focus();
//}


// these two functions are used to also expand everything up the spec+filename
function goto_spec(spec_filename) {
  if (path["_"+spec_filename]){
    // deals with multiple inheritance
    var paths = path["_"+spec_filename].split("|");
    for (i=0; i<paths.length; i++) {
      if (i == (paths.length-1)) {
	  expand_path(paths[i],true);
      }else {
	  expand_path(paths[i],false)
      }
      //alert(i);
    }
  }
  //alert(spec_filename);
  goToSpec(spec_filename);
  //select_and_showSpec(spec_filename);
}

// these two functions are used to also expand everything up the spec+filename
function goto_system_spec(spec_filename) {
  if (path["_"+spec_filename]){
    // deals with multiple inheritance
    var paths = path["_"+spec_filename].split("|");
    for (i=0; i<paths.length; i++) {
      if (i == (paths.length-1)) {
	  expand_path(paths[i],true);
      }else {
	  expand_path(paths[i],false)
      }
    }
  }
  goToSystemSpec(spec_filename);
}

// Same for KM file
function goto_km(km_filename){
  if (path["_"+km_filename]) {
    // deals with multiple inheritance
    var paths = path["_"+km_filename].split("|");
    for (i=0; i<paths.length; i++) {
      if (i == (paths.length-1)) {
	  expand_path(paths[i],true);
      }else {
	  expand_path(paths[i],false)
      }
      //alert(i);
    }
  }
  select_and_showKM(km_filename);
}

function expand_path(pathString, expand) {
  var prefix = new String('parent.frames[\'code\'].MTMSubAction(');
  var cmd_root = new String('parent.frames[\'code\'].menu.items[0]');
  var pathArray = pathString.split(",");
  var cmd = cmd_root;
 
  //eval(cmd_root + '.expanded = true');
  //alert(cmd_root + '.expanded = true');
  if (!eval (cmd_root + '.expanded') ) { 
    //alert(prefix + cmd_root + ",false)");
    //eval (prefix + cmd_root + ",false)");
    eval(cmd_root + '.expanded = true');
  }

  for (j=1; j<pathArray.length - 1; j++){
    cmd += ".submenu.items[" + pathArray[j] + "]";
    if (!eval (cmd + '.expanded')) {
       //eval (cmd + '.expanded = true');
       //alert(cmd + '.expanded');
       //eval(prefix + cmd + ",false)");
       eval (cmd + '.expanded = true');
       //alert(prefix + cmd + ",false)");
    } 
  }

  //if (!eval (cmd_root + '.expanded') ) { 
    //alert(prefix + cmd_root + ",false)");
    //eval (prefix + cmd_root + ",false)");
  //}

  if (expand) {
    cmd += ".submenu.items[" + pathArray[j] + "]";
    eval(prefix+cmd+",false)");
  }
  //var expand_cmd = prefix + cmd_root + ",false);";
  //alert(expand_cmd);
  //eval(expand_cmd);
}





//************************************************************************
// Code for displaying last visited concepts



// Warning does not remove duplicates
function update_visited(name) {
  //alert(name);
  
  for (i=0; i< visitedNo; i++) { 
    if (recentlyVisited[i] == name) { return;}
  }

  if (visitedNo > 0) {tmp = recentlyVisited[0];}
  if (visitedNo < max_visited) {visitedNo++;}
  for (j=visitedNo; j>=2; j--){
    recentlyVisited[j] = recentlyVisited[j-1];
  }
  recentlyVisited[0]=name;
  if (visitedNo>1) {recentlyVisited[1]=tmp;}
  parent.heading.update_visited_select();
}

