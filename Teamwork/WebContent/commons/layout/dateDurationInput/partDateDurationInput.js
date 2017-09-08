  function resynchDates(fieldName, startTaskId, startIsMilesId, durTaskId, endTaskId, endIsMilesId) {
    //console.debug("resynchDates",fieldName);

    var durTask = $("#" + durTaskId);
    var startTask = $("#" + startTaskId);
    var endTask = $("#" + endTaskId);
    var startIsMiles = $("#" + startIsMilesId);
    var endIsMiles = $("#" + endIsMilesId);


    function resynchDatesSetFields(command) {
      //var duration = parseInt(durTask.val());
      var duration = daysFromString(durTask.val(),true);
      if (!duration || duration < 1)
        duration = 1;

      var date = Date.parseString(startTask.val());

      while (isHoliday(date)) {
        date.setDate(date.getDate() + 1);
      }

      var start = date.getTime();

      var end = endTask.val();
      if (end.length > 0) {
        date = Date.parseString(end);
        while (isHoliday(date)) {
          date.setDate(date.getDate() + 1);
        }
        end = date.getTime();
      }

      if ("CHANGE_END" == command) {
        date.setTime(start);
        var workingDays = duration - 1;
        date.incrementDateByWorkingDays(workingDays);
        endTask.val(date.format());
      } else if ("CHANGE_START" == command) {
        date.setTime(end);
        var workingDays = duration - 1;
        date.incrementDateByWorkingDays(-workingDays);
        startTask.val(date.format());
      } else if ("CHANGE_DURATION" == command) {
        //console.debug("CHANGE_DURATION",new Date(start),new Date(end))
        durTask.val(new Date(start).distanceInWorkingDays(new Date(end))+1);
      }
    }


    var durIsFilled = durTask.val().length > 0;
    var startIsFilled = startTask.val().length > 0;
    var endIsFilled = endTask.val().length > 0;

    var startIsMilesAndFilled = startIsFilled && (startIsMiles.val() == 'yes' || startTask.is("[readOnly]"));
    var endIsMilesAndFilled = endIsFilled && (endIsMiles.val() == 'yes' || endTask.is("[readOnly]"));

    if (durIsFilled) {
      //if (parseInt(durTask.val()) == NaN || parseInt(durTask.val()) < 1)
      if (!daysFromString(durTask.val()))
        durTask.val(1);
    }

    if (fieldName == 'MILES') {
      if (startIsMilesAndFilled && endIsMilesAndFilled) {
        durTask.prop("readOnly", true);
        durTask.css("background-color", "#f3f3f3");
      } else {
        durTask.prop("readOnly", false);
        durTask.css("background-color", "#ffffff");
      }
      return;
    }

    //need at least two values to resynch the third
    if ((durIsFilled ? 1 : 0) + (startIsFilled ? 1 : 0) + (endIsFilled ? 1 : 0) < 2)
      return;

    if (fieldName == 'START' && startIsFilled) {
      if (endIsMilesAndFilled && durIsFilled) {
        resynchDatesSetFields("CHANGE_DURATION");
      //} else if (durIsFilled) { // 27/11/2015 deve fare il calcolo comunque, tanto se non c'è si considera "1"
      } else {
        resynchDatesSetFields("CHANGE_END");
      }

    } else if (fieldName == 'TASK_DURATION' && durIsFilled && !(endIsMilesAndFilled && startIsMilesAndFilled)) {
      if (endIsMilesAndFilled && !startIsMilesAndFilled) {
        resynchDatesSetFields("CHANGE_START");
      } else if (!endIsMilesAndFilled) {
        //document.title=('go and change end!!');
        resynchDatesSetFields("CHANGE_END");
      }

    } else if (fieldName == 'END' && endIsFilled) {
      resynchDatesSetFields("CHANGE_DURATION");
    }
  }
