var NodesController = function($scope, $http, $timeout) {
  $scope.nodes = [];

  $scope.fetch = function() {
    $http.get('/nodes.json').
      success(function(data, status, headers, config) {
        $scope.nodes = data;
      });
  };

  $scope.reload = function() {
    $scope.fetch();
    $timeout($scope.reload, 1000);
  };

  $scope.reload();
};
