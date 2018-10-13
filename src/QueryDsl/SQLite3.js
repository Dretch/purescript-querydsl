
exports.decodeQueryResponse = function (addString, addInt, addNumber, addNull, empty, results) {
  return results.map(function(result) {

    var decoded = empty;
    Object.keys(result).forEach(function(key) {

      var value = result[key];

      if (value instanceof String) {
        decoded = addString(key)(value)(decoded);
      }
      else if (typeof value === 'number') {
         var add = Number.isInteger(value) ? addInt : addNumber;
         decoded = add(key)(value)(decoded);
      }
      else if (!value) {
        decoded = addNull(key)(decoded);
      }
      else {
        throw new Error("Unable to decode value: " + value);
      }
    });
    return decoded;
  });
};
