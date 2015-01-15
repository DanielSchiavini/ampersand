AmpersandApp.controller('PeopleController', ['$scope', '$rootScope', '$routeParams', 'Restangular', '$timeout', '$modal', function ($scope, $rootScope, $routeParams, Restangular, $timeout, $modal) {
	
	// model (can be changed by view)
	$scope.SESSION = Restangular.one('interface/People/atom', $routeParams.atom).get().$object;
	
	// patch function
	$scope.patch = function(){
		$scope.SESSION
			.patch()
			.then(function(data) {
				$rootScope.notifications = data.notifications;
				$scope.SESSION = Restangular.restangularizeElement('', data.content, 'interface/People/atom');
				
				$timeout(function() {
			    	console.log('now');
			    	$rootScope.notifications.successes = [];
			    }, 3000);
			});
		
	}
	
	// function to remove item (key) from list (obj)
	$scope.removeObject = function(obj, key){
		delete obj[key];
		$scope.patch();
	}

}]);
