  $(function() {
    $(".schedComposer").each(function(){
      var schedComposer=$(this);
      var inp=schedComposer.find(".scMainField");
      if (inp.val()){
        var json;
        eval("json="+inp.val());
        fillFromJSON(schedComposer,json);
      } else {
        schedComposer.find("[schedType=period]").click();
      }
      inp.updateOldValue();
    });
  });


  function changeSchedType(el) {
    var type = el.attr("schedType");
    var schedComp = el.closest(".schedComposer");
    schedComp.find("[schedType]").removeClass("focused");
    el.addClass("focused");
    changePanel(schedComp,type);
    jsonifySchedule(el);
  }


  function changePanel(schedComp,type) {
    schedComp.attr("currType",type);

    //by default hide all recurrencies and the end date
    schedComp.find("[recurr],#sc_endDate").hide();
    schedComp.find("#sc_fullDay").show();

    //period
    if (type=="period"){
      schedComp.find("#sc_endDate").show();

    //minute
    } else if (type=="minute"){
      schedComp.find("#sc_fullDay").hide();
      schedComp.find("[recurr=minute]").show();

    //daily
    } else if (type=="daily"){
      schedComp.find("[recurr=daily]").show();

    //weekly
    } else if (type=="weekly"){
      schedComp.find("[recurr=weekly]").show();

    //monthly
    } else if (type=="monthly"){
      schedComp.find("[recurr=monthly]").show();

    //yearly
    } else if (type=="yearly"){
      schedComp.find("[recurr=yearly]").show();
    }

  }



  function jsonifySchedule(el) {
    //console.debug("jsonifySchedule");
    var schedComposer = el.closest(".schedComposer");
    var type=schedComposer.attr("currType");
    var fields=schedComposer.find(".sc_period,[recurr="+type+"]").getScheduleFields();
    var fieldLeftName = el.prop("name");
    var period=setStartEndDateHourMinute(fieldLeftName,fields);
    var json={
      type:type,
      startMillis:period.startMillis,
      duration:period.duration,
      endMillis:period.endMillis
    };

    if (type!="period"){

      var freq=parseInt(fields.freq.val());
      freq=freq>0?freq:1;
      json.freq=freq;
      fields.freq.val(freq);

      var repeat=parseInt(fields.repeat.val());
      repeat=repeat>=0?repeat:1;
      json.repeat=repeat;
      fields.repeat.val(repeat);
      
      if (type=="minute"){
        json.onlyWorkingDays=fields.onlyWorkingDays.is(":checked");

      } else if (type=="daily"){
        json.onlyWorkingDays=fields.onlyWorkingDays.is(":checked");

      } else if (type=="weekly"){
        var days=[];
        schedComposer.find(":checkbox[day]:checked").each(function(){
          days.push($(this).attr("day"));                
        });
        json.days=days;

      } else if (type=="monthly"){
        if (fields.recurType && fields.recurType.val()==2){
          json.weekOfMonth=fields.weekOfMonth.val();
          json.dayOfWeek=fields.dayOfWeek.val();          
        }
        schedComposer.find(".monthlyDate").html(new Date(period.startMillis).format("dd"));

      } else if (type=="yearly"){
        if (fields.recurType && fields.recurType.val()==2){
          json.weekOfMonth=fields.weekOfMonth.val();
          json.dayOfWeek=fields.dayOfWeek.val();
        }
        schedComposer.find(".yearlyDate1").html(new Date(period.startMillis).format("dd MMMM"));
        schedComposer.find(".yearlyDate2").html(new Date(period.startMillis).format("MMMM"));
      }
    //} else {
    //  json.endMillis=period.endMillis;

    }
    schedComposer.find(".scMainField").val(JSON.stringify(json));
  }


  function setStartEndDateHourMinute(fieldLeftName, fields) {
    var startDate = fields.startDate.val();
    startDate = Date.parseString(startDate) ;
    startDate = startDate ? startDate : new Date().clearTime();

    var startTime = fields.startHour.val();
    startTime = millisFromString(startTime);
    startTime = typeof(startTime)=="number"  ? startTime : millisFromString(new Date().format("HH:mm"));


    var endDate;
    if (fields.endDate)
      endDate = fields.endDate.val();
    endDate = Date.parseString(endDate);
    endDate = endDate ? endDate : new Date().clearTime();

    var endTime;
    if (fields.endHour)
      endTime = fields.endHour.val();
    endTime =  millisFromString(endTime) ;
    endTime =  typeof(endTime)=="number"?endTime: millisFromString(new Date().format("HH:mm"));

    var dur = fields.duration.val();
    dur = dur ? millisFromString(dur) : 30 * 60000;

    dur = dur<=60000?60000:dur;
    //console.debug(startDate,endDate,startTime,endTime,dur);

    var startMillis = startDate.getTime() + startTime;
    var endMillis = endDate.getTime() + endTime;

    if (fieldLeftName) {
      if (fieldLeftName.indexOf("start") > -1) {
        endMillis = startMillis + dur;


      } else if (fieldLeftName.indexOf("end") > -1) {
        dur = endMillis - startMillis;
        if (dur < 60000) {
          dur = 60000;
          startMillis=endMillis-60000;
        }

      } else if (fieldLeftName.indexOf("duration") > -1) {
        endMillis = startMillis + dur;
      }
    }

    startDate = new Date(startMillis);
    endDate = new Date(endMillis);

    fields.startDate.val(startDate.format());
    fields.startHour.val(startDate.format("HH:mm"));

    fields.duration.val(getMillisInHoursMinutes(dur));

    if (fields.endDate)
      fields.endDate.val(endDate.format());
    if (fields.endHour)
      fields.endHour.val(endDate.format("HH:mm"));

    return {startMillis:startMillis,endMillis:endMillis,duration:dur};
  }


  function fillFromJSON(schedComposer,json){
    //console.debug("fillFromJSON",json);
    changePanel(schedComposer,json.type);

    var start= new Date(json.startMillis);
    var end= new Date(json.endMillis);

    var fields=schedComposer.find(".sc_period,[recurr=\""+json.type+"\"]").getScheduleFields();
    fields.startDate.val(start.format());
    fields.startHour.val(start.format("HH:mm"));
    if (fields.endDate)
      fields.endDate.val(end.format());
    if (fields.endHour)
      fields.endHour.val(end.format("HH:mm"));
    fields.duration.val(getMillisInHoursMinutes(json.duration));
    if(fields.freq && json.freq != undefined)
      fields.freq.val(json.freq);
    if(fields.repeat && json.repeat != undefined)
      fields.repeat.val(json.repeat);
    if(fields.onlyWorkingDays && json.onlyWorkingDays)
      fields.onlyWorkingDays.prop("checked",json.onlyWorkingDays);
    if (json.days){
      for (var i=0;i<json.days.length;i++){
        var day=json.days[i];
        fields["day"+day].prop("checked",true);
      }
    }

    if ( json.dayOfWeek){
      fields.dayOfWeek.val(json.dayOfWeek);
    }

    schedComposer.find("[name=recurType][value=1]:visible").prop("checked",true);
    if ( json.weekOfMonth){
      fields.weekOfMonth.val(json.weekOfMonth);
      schedComposer.find("[name=recurType][value=2]:visible").prop("checked",true);
    }
    schedComposer.find("[schedType="+json.type+"]").click();


  }


  $.fn.getScheduleFields = function() {
    var fields = {};
    this.find(":input").each(function() {
      var el = $(this);
      if (el.prop("name")) {
        if (el.is(":radio")) {
          if (el.is(":checked"))
            fields[el.prop("name")] = el;
        } else
          fields[el.prop("name")] = el;
      }
    });
//    console.debug(this,fields)
    return fields;
  };



  
  function setFullDay(el) {
    var periodPane = el.closest(".sc_period");
    var fields = periodPane.getScheduleFields();
    fields.startHour.val('00:00');
    fields.endHour.val('23:59');
    jsonifySchedule(fields.endDate);
  }
