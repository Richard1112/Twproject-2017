// ----------------------------------  PROFILING ------------------------------------------
var __profiler = {};
/**
 * usage: instantiate a new Profiler("a name") at the beginning of the code you want to profile  var p= new Profiler("testLoop")
 *        call p.stop() at the end of the code you want test.
 *        call Profiler.displayAll() or p.display() to see how many times the code has been executed and millisecond spent.
 *        call Profiler.resetAll() or p.reset() to restart profiler.
 * @param name
 */
function Profiler(name) {
	this.startTime = new Date().getTime();
	this.name = name;

	this.stop = function () {
		if (!__profiler[this.name])
			__profiler[this.name] = {millis:0, count:0};
		__profiler[this.name].millis += new Date().getTime() - this.startTime;
		__profiler[this.name].count++;
	};
	this.display = function () {
		console.debug(__profiler[this.name]);
	};

	this.reset = function () {
		delete __profiler[this.name];
	};
}

Profiler.reset = function () {
	__profiler = {};
};

Profiler.displayAll = function () {
	var ret = "";
	var totMillis = 0;
	for (var key in  __profiler) {
		var p = __profiler[key];
		var extraspace = "                          ".substr(0, 30 - key.length);
		ret += key + extraspace + "\t millis:" + p.millis + "\t count:" + p.count + "\n";
		totMillis += p.millis;
	}
	console.debug(ret);
};

