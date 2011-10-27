<?php

/* TODO

-debug just puts elements in the page, should be in a specific place
-errsors are in nasty globarl $DB_errss
-fix require "dbsettings.php". just declare a constant that returns the settings, and do the connection somewhere else
-check why DB_doquer creates 0 and tgt fields for each row (even when there is only 1 column)
-rewrite DB_doquer
-Do we want objects instead of arrays? do they add anything?
- escape correctly (also need to use htmlSpecialChars?)
*/

require "dbsettings.php";
  
$DB_errs = array();
    
function firstRow($rows) {
  return $rows[0];
}
  
function firstCol($rows) {
  foreach ($rows as $i=>$v) $v=$v[0]; return $rows;
}

function targetCol($rows) {
  foreach ($rows as $i=>&$v) $v=$v['tgt']; return $rows;
}

function printBinaryTable($table) {
  echo '<table>';
  foreach ($table as $row)
    echo '<tr><td>'.$row['src'].'</td><td>'.$row['tgt'].'</td></tr>';
  echo '</table>';
}

function printArray($arr) {
  foreach ($arr as $v)
    echo $v.'</br>';
}

function DB_debug($txt,$lvl=0){
  global $DB_debug;
  if ($lvl<=$DB_debug) {
    echo "<i title=\"debug level $lvl\">$txt</i>\n<P />\n";
    return true;
  }
  else
    return false;
}
  
  
function DB_doquer($DbName, $quer,$debug=5)
{
  global $DB_link,$DB_errs;
  $DB_slct = mysql_select_db($DbName,$DB_link);
    
  DB_debug($quer,$debug);
  $result=mysql_query($quer,$DB_link);
  if(!$result){
    DB_debug('Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'": '.mysql_error(),2);
    $DB_errs[]='Error '.($ernr=mysql_errno($DB_link)).' in query "'.$quer.'"';
    return false;
  }
  if($result===true) return true; // succes.. but no contents..
  $rows=Array();
  while (($row = @mysql_fetch_array($result))!==false) {
    $rows[]=$row;
    unset($row);
  }
  return $rows;
}


///////// Interface stuff (does not belong here) /////////

function generateInterfaceList($db, $interfaces, $atom) {
  $html = "";
  if (count($interfaces) == 0)
    emit($html, withClass('Atom',$atom));
  else {
    emit($html, withClass('Atom',$atom)); // just for debugging
    emit($html, '<div class=Interface>');
    foreach($interfaces as $interface) {
      emit($html, generateInterface($db, $interface, $atom));
    }
    emit($html, '</div>');
  }
  return $html;
}

function generateInterface($db, $interface, $srcAtom) {
  $html = "";
  emit($html, withClass('Label', $interface['name']));
  $codomainAtoms = getCoDomainAtoms($db, $srcAtom, $interface['sqlQuery']);
  //print_r($codomainAtoms);
  
  $isUni = $interface['isUnivalent'];  
  
  if (!$isUni) emit($html, '<div class=AtomList><ul>');
  else         emit($html, '<div class=Atomic>');
  foreach($codomainAtoms as $tgtAtom) {
    if (!$isUni) emit($html, '<li>');
    emit($html, generateInterfaceList($db, $interface['subInterfaces'], $tgtAtom));
    if (!$isUni) emit($html,'</li>'); 
  }
  if (!$isUni) emit($html, '</ul></div>');
  else         emit($html, '</div>');
  return $html;
}

function getCoDomainAtoms($db, $atom, $selectRel) {
  return targetCol(DB_doquer($db, selectCoDomain($atom, $selectRel)));
}

function selectCoDomain($atom, $selectRel) {
  return 'SELECT DISTINCT `tgt` FROM ('.$selectRel.') as results where src=\''.$atom.'\'';
}


// utils

function withClass($class, $elt) {
  return "<div class=$class>$elt</div>";
}

function emit(&$lines,$line) {
  $lines.=$line."\n";
}

?>