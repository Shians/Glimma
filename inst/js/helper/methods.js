// Method to get unique elements of array.
// REQUIRES D3
if (typeof typeof d3 != "undefined") {
	Array.prototype.unique = function () {
		return d3.set(this).values();
	}	
}
