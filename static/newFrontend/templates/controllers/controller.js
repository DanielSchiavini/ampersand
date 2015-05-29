/*
Controller for interface "$interfaceName$" (context: "$contextName$"). Generated code, edit with care.
Generated by $ampersandVersionStr$

INTERFACE "$interfaceName$" : $expAdl$ :: $source$ * $target$  ($if(!isRoot)$non-$endif$root interface)
Roles: [$roles;separator=", "$]
Editable relations: [$editableRelations;separator=", "$] 
*/

AmpersandApp.controller('$interfaceName$Controller', function (\$scope, \$rootScope, \$location, \$routeParams, Restangular, \$location) {
  
  \$scope.val = {};
  \$scope.initialVal = {};
  \$scope.showSaveButton = {}; // initialize showSaveButton object
  
  // BaseURL to the API is already configured in AmpersandApp.js (i.e. 'http://pathToApp/api/v1/')
  
  // Only insert code below if interface is allowed to create new atoms. This is not specified in interfaces yet, so add by default
  if(\$routeParams['new']){
	\$scope.val['$interfaceName$'] = Restangular.one('resource/SESSION', \$rootScope.session.id).all('$interfaceName$').post({}).\$object;
    
  // Elseif resourceId is provided
  }else if(typeof \$routeParams.resourceId != 'undefined'){
    \$scope.val['$interfaceName$'] = Restangular.one('resource/$source$', \$routeParams.resourceId).getList('$interfaceName$').\$object;
  
  // Else use session.id
  }else{
	\$scope.val['$interfaceName$'] = Restangular.one('resource/$source$', \$rootScope.session.id).getList('$interfaceName$').\$object;
  }
  
  \$scope.\$on("\$locationChangeStart", function(event, next, current) { 
    console.log("location changing to:" + next);
    
    checkRequired = false; // default
    for(var item in \$scope.showSaveButton) { // iterate over all properties (resourceIds) in showSaveButton object
      if(\$scope.showSaveButton.hasOwnProperty( item ) ) { // only checks its own properties, not inherited ones
        if(\$scope.showSaveButton[item] == true) checkRequired = true; // if item is not saved, checkRequired before location change
      }
    }
    
    if(checkRequired){ // if checkRequired (see above)
    	confirmed = confirm("You have unsaved edits. Do you wish to leave?");
        if (event && !confirmed) { 
          event.preventDefault();
        }
    }
  });


  // The function below is only necessary if the interface allows to delete the complete atom,
  // but since this cannot be specified yet in Ampersand we always include it.

  // Delete function to delete a complete Resource
  \$scope.deleteResource = function (ResourceId){
    if(confirm('Are you sure?')){
      \$scope.val['$interfaceName$'][ResourceId]
        .remove({ 'requestType' : 'promise'})
        .then(function(data){
          \$rootScope.updateNotifications(data.notifications);
          \$scope.val['$interfaceName$'].splice(ResourceId, 1); // remove from array
        });
    }
  }
   
$if(containsDATE)$  // The interface contains an editable relation to the primitive concept DATE
  // Function for Datepicker
  \$scope.datepicker = []; // empty array to administer if datepickers (can be multiple on one page) are open and closed
  \$scope.openDatepicker = function(\$event, datepicker) {
    \$event.preventDefault();
    \$event.stopPropagation();
    
    \$scope.datepicker[datepicker] = {'open' : true};
  }
$else$  // The interface does not contain editable relations to primitive concept DATE
$endif$
$if(containsEditable)$  // The interface contains at least 1 editable relation
  // Put function to update a Resource
  \$scope.put = function(ResourceId, requestType){
	requestType = requestType || 'feedback'; // this does not work if you want to pass in a falsey value i.e. false, null, undefined, 0 or ""
    \$scope.val['$interfaceName$'][ResourceId]
      .put({'requestType' : requestType})
      .then(function(data) {
        \$rootScope.updateNotifications(data.notifications);
        \$scope.val['$interfaceName$'][ResourceId] = \$.extend(\$scope.val['$interfaceName$'][ResourceId], data.content);
        
        // show/hide save button
        if(data.invariantRulesHold && data.requestType == 'feedback'){ // if invariant rules hold (promise is possible) and the previous request was not a request4feedback (i.e. not a request2promise itself)
        	\$scope.showSaveButton[ResourceId] = true;
        }else{
        	\$scope.showSaveButton[ResourceId] = false;
        }
      });
  }

  // Function to patch only the changed attributes of a Resource
  \$scope.patch = function(ResourceId){
	  patches = diff(\$scope.initialVal['$interfaceName$'][ResourceId], \$scope.val['$interfaceName$'][ResourceId]) // determine patches
	  console.log(patches);
	  
	  /*
	  \$scope.val['$interfaceName$'][ResourceId]
	  .patch(patches)
	  .then(function(data) {
		 \$rootScope.updateNotifications(data.notifications);
		 \$scope.val['$interfaceName$'][ResourceId] = Restangular.restangularizeElement('', data.content, url);
	  });
	  */
  }
  
  // Function to add item to array of primitieve datatypes
  \$scope.addItem = function(obj, property, selected, ResourceId){
    if(selected.value != ''){
      if(obj[property] === null) obj[property] = [];
      obj[property].push(selected.value);
      selected.value = '';
      \$scope.put(ResourceId);
    }else{
    	console.log('Empty value selected');
    }
  }
  
  //Function to remove item from array of primitieve datatypes
  \$scope.removeItem = function(obj, key, ResourceId){
    obj.splice(key, 1);
    \$scope.put(ResourceId);
  }
$else$  // The interface does not contain any editable relations
$endif$
$if(containsEditableNonPrim)$  // The interface contains at least 1 editable relation to a non-primitive concept
  // AddObject function to add a new item (val) to a certain property (property) of an object (obj)
  // Also needed by addModal function.
  \$scope.addObject = function(obj, property, selected, ResourceId){
    if(selected.id === undefined || selected.id == ''){
      console.log('selected id is undefined');
    }else{
      if(obj[property] === null) obj[property] = {};
      obj[property][selected.id] = {'id': selected.id};
      selected.id = ''; // reset input field
      \$scope.put(ResourceId);
    }
  }
  
  // RemoveObject function to remove an item (key) from list (obj).
  \$scope.removeObject = function(obj, key, ResourceId){
    delete obj[key];
    \$scope.put(ResourceId);
  }
  
  // Typeahead functionality
  \$scope.typeahead = {}; // an empty object for typeahead


  // A property for every (non-primitive) tgtConcept of the editable relations in this interface
  $editableNonPrimTargets:{concept|\$scope.typeahead['$concept$'] = Restangular.all('resource/$concept$').getList().\$object;
  }$
$else$  // The interface does not contain editable relations to non-primitive concepts
$endif$
});
