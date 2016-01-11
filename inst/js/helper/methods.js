//* REQUIRES D3 *//
// Method to get unique elements of array.
if (typeof typeof d3 != "undefined") {
	if (typeof Array.prototype.unique !== "function") {
		Array.prototype.unique = function () {
			return d3.set(this).values();
		};
	}
}
