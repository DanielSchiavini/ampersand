<?php 
/* This file defines the function 'TransitiveClosure', that computes the transitive closure of a relation using Washall's algorithm.
   There are no guarantees with respect to its 100% functioning. Have fun...

   Suppose you need the transitive closure r* of a relation r :: C * C
   This pattern tells you how to define a relation rStar that contains the same population as r*
   (which you need, but isn't provided in the prototype generator, so this is the workaround).
   Maintaining the population of rStar correctly is not trivial, particularly when r is depopulated.
   The easiest way around this is to compute rStar from scratch.
   However, you then need to know that r is being (de)populated, so we need a copy of r.
   This leads to the following pattern:
   
   relation :: Concept*Concept
   relationCopy :: Concept*Concept -- copied value of 'relation' allows for detecting modifcation events
   relationStar :: Concept*Concept -- transitive closure of relation
   
   ROLE ExecEngine MAINTAINS "relationCompTransitiveClosure"
   RULE "relationCompTransitiveClosure": relation = relationCopy
   VIOLATION (TXT "{EX} TransitiveClosure;relation;Concept;relationCopy;relationStar")

   NOTES:
   1) The above example is made for ease of use. This is what you need to do:
      a) copy and paste the above example into your own ADL script;
      b) replace the names of 'relation' and 'Concept' (cases sensitive, also as part of a word) with what you need
      c) make sure you define an INTERFACE that contains both 'relationCopy' and 'relationStar'
         (this is necessary for interfacing with such relations using PHP).
   2) Of course, there are all sorts of alternative ways in which 'TransitiveClosure' can be used.
   3) There are ways to optimize the below code, e.g. by splitting the function into an 'InsTransitiveClosure'
      and a 'DelTransitiveClosure'
*/

function TransitiveClosure($r,$C,$rCopy,$rStar){
	Notifications::addLog("Exeucte TransitiveClosure($r,$C,$rCopy,$rStar)");

	if($GLOBALS['ext']['ExecEngine']['functions']['warshall']['warshallRuleChecked'][$r]){
		Notifications::addLog("Skipping TransitiveClosure($r,$C,$rCopy,$rStar)");
		return;  // this is the case if we have executed this function already in this transaction		
	}else{
		
		$GLOBALS['ext']['ExecEngine']['functions']['warshall']['warshallRuleChecked'][$r] = true;
		
		// Compute transitive closure following Warshall's algorithm
		$closure = RetrievePopulation($r, $C); // get adjacency matrix
		
		OverwritePopulation($closure, $rCopy, $C); // store it in the 'rCopy' relation
		
		// Get all unique atoms from this population
		$atoms = array_keys($closure); // 'Src' (left) atoms of pairs in $closure
		
		foreach ($closure as $tgtAtomsList){ // Loop to add 'Tgt' atoms that not yet exist
			$tgtAtoms = array_keys($tgtAtomsList);
			foreach ($tgtAtoms as $tgtAtom){
				if (!in_array($tgtAtom, $atoms)) $atoms[] = $tgtAtom;
			}
		}
		
		foreach ($atoms as $k){
			foreach ($atoms as $i){
				if ($closure[$i][$k]){
					foreach ($atoms as $j){
						$closure[$i][$j] = $closure[$i][$j] || $closure[$k][$j];
					}
				}
			}
		}
		
		OverwritePopulation($closure, $rStar, $C);
	}
}

function RetrievePopulation($relationName, $concept){
	try{
		$database = Database::singleton();
		
		$fullRelationSignature = Relation::isCombination($relationName, $concept, $concept);
		$table = Relation::getTable($fullRelationSignature);
		$srcCol = Relation::getSrcCol($fullRelationSignature);
		$tgtCol = Relation::getTgtCol($fullRelationSignature);
		
		$query = "SELECT * FROM $table";
		$result = $database->Exe($query);
		
		// initialization of 2-dimensional array
		foreach($result as $row){
			$array[$row['src']][$row['tgt']] = true;
		}
		
		return (array)$array;
	}catch(Exception $e){
		throw new Exception('RetrievePopulation: ' . $e->getMessage(), 500);
	}
}

// Overwrite contents of &-relation $r with contents of php array $rArray
function OverwritePopulation($rArray, $relation, $concept){
	try{
		$database = Database::singleton();
		
		$fullRelationSignature = Relation::isCombination($relationName, $concept, $concept);
		$table = Relation::getTable($fullRelationSignature);
		$srcCol = Relation::getSrcCol($fullRelationSignature);
		$tgtCol = Relation::getTgtCol($fullRelationSignature);
		
		$query = "TRUNCATE TABLE $table";
		$database->Exe($query);
		
		foreach($rArray as $src => $tgtArray){
			foreach($tgtArray as $tgt => $bool){
				if($bool){
					$query = "INSERT INTO $table (`$srcCol`, `$tgtCol`) VALUES ('$src','$tgt')";
					$database->Exe($query);
				}
			}
		}
		
	}catch(Exception $e){
		throw new Exception('OverwritePopulation: ' . $e->getMessage(), 500);
	}
}
?>