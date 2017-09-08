<%@ page contentType="text/javascript"%><%@ page import="net.sf.json.JSONArray, org.jblooming.agenda.CompanyCalendar, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.NumberUtilities, org.jblooming.waf.SessionState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.text.DecimalFormatSymbols" %>
  <%
  PageState pageState = PageState.getCurrentPageState(request);
  int a=1;
  if (false){
    %><script type='text/javascript'><%
}%>
  function dateToRelative(localTime){
    var diff=new Date().getTime()-localTime;
    var ret="";

    var min=<%=CompanyCalendar.MILLIS_IN_MINUTE%>;
    var hour=<%=CompanyCalendar.MILLIS_IN_HOUR%>;
    var day=<%=CompanyCalendar.MILLIS_IN_DAY%>;
    var wee=<%=CompanyCalendar.MILLIS_IN_WEEK%>;
    var mon=<%=CompanyCalendar.MILLIS_IN_MONTH%>;
    var yea=<%=CompanyCalendar.MILLIS_IN_YEAR%>;

    if (diff<-yea*2)
    ret ="<%=I18n.get("DATEREL_IN_%%_YEARS","##")%>".replace("##",(-diff/yea).toFixed(0));

    else if (diff<-mon*9)
    ret ="<%=I18n.get("DATEREL_IN_%%_MONTHS","##")%>".replace("##",(-diff/mon).toFixed(0));

    else if (diff<-wee*5)
    ret ="<%=I18n.get("DATEREL_IN_%%_WEEKS","##")%>".replace("##",(-diff/wee).toFixed(0));

    else if (diff<-day*2)
    ret ="<%=I18n.get("DATEREL_IN_%%_DAYS","##")%>".replace("##",(-diff/day).toFixed(0));

    else if (diff<-hour)
    ret ="<%=I18n.get("DATEREL_IN_%%_HOURS","##")%>".replace("##",(-diff/hour).toFixed(0));

    else if (diff<-min*35)
    ret ="<%=I18n.get("DATEREL_IN_ABOUT_1_HOUR")%>";

    else if (diff<-min*25)
    ret ="<%=I18n.get("DATEREL_IN_ABOUT_HALF_HOUR")%>";

    else if (diff<-min*10)
    ret ="<%=I18n.get("DATEREL_IN_SOME_MINUTES")%>";

    else if (diff<-min*2)
    ret ="<%=I18n.get("DATEREL_IN_FEW_MINUTES")%>";

    else if (diff<=min)
    ret ="<%=I18n.get("DATEREL_JUSTNOW")%>";

    else if (diff<=min*5)
    ret ="<%=I18n.get("DATEREL_FEW_MINUTES_AGO")%>";

    else if (diff<=min*15)
    ret ="<%=I18n.get("DATEREL_SOME_MINUTES_AGO")%>";

    else if (diff<=min*35)
    ret ="<%=I18n.get("DATEREL_ABOUT_HALF_HOUR_AGO")%>";

    else if (diff<=min*75)
    ret ="<%=I18n.get("DATEREL_ABOUT_1_HOUR_AGO")%>";

    else if (diff<=hour*5)
    ret ="<%=I18n.get("DATEREL_FEW_HOURS_AGO")%>";

    else if (diff<=hour*24)
    ret ="<%=I18n.get("DATEREL_%%_HOURS_AGO","##")%>".replace("##",(diff/hour).toFixed(0));

    else if (diff<=day*7)
    ret ="<%=I18n.get("DATEREL_%%_DAYS_AGO","##")%>".replace("##",(diff/day).toFixed(0));

    else if (diff<=wee*5)
    ret ="<%=I18n.get("DATEREL_%%_WEEKS_AGO","##")%>".replace("##",(diff/wee).toFixed(0));

    else if (diff<=mon*12)
    ret ="<%=I18n.get("DATEREL_%%_MONTHS_AGO","##")%>".replace("##",(diff/mon).toFixed(0));

    else
    ret ="<%=I18n.get("DATEREL_%%_YEARS_AGO","##")%>".replace("##",(diff/yea).toFixed(0));

    return ret;
    }

  //override date format i18n
  <%

    JSONArray dayNames= new JSONArray();
    JSONArray monthNames= new JSONArray();
    JSONArray shortDayNames= new JSONArray();
    JSONArray shortMonthNames= new JSONArray();
    CompanyCalendar cc= new CompanyCalendar(SessionState.getLocale());

    // day names
    cc.set(CompanyCalendar.DAY_OF_WEEK,CompanyCalendar.SUNDAY);
    for (int i = 0;i<7;i++){
      shortDayNames.add(cc.format("EEE"));
      dayNames.add(cc.format("EEEE"));
      cc.add(CompanyCalendar.DATE,1);
    }


    //month names
    cc.set(CompanyCalendar.MONTH,CompanyCalendar.JANUARY);
    for (int i = 0;i<12;i++){
      shortMonthNames.add(cc.format("MMM"));
      monthNames.add(cc.format("MMMM"));
      cc.add(CompanyCalendar.MONTH,1);
    }


    DecimalFormatSymbols symbols=DecimalFormatSymbols.getInstance(SessionState.getLocale());  //todo jdk6

%>
  Date.monthNames = <%=monthNames%>;
  // Month abbreviations. Change this for local month names
  Date.monthAbbreviations = <%=shortMonthNames%>;
  // Full day names. Change this for local month names
  Date.dayNames =<%=dayNames%>;
  // Day abbreviations. Change this for local month names
  Date.dayAbbreviations = <%=shortDayNames%>;
  // Used for parsing ambiguous dates like 1/2/2000 - default to preferring 'American' format meaning Jan 2.
  // Set to false to prefer 'European' format meaning Feb 1
  Date.preferAmericanFormat = false;

  Date.firstDayOfWeek =<%=new CompanyCalendar(SessionState.getLocale()).getFirstDayOfWeek()-1%>;
  Date.defaultFormat = "<%= DateUtilities.getFormat(DateUtilities.DATE_DEFAULT)%>";
  Date.masks = {
    fullDate:       "<%=DateUtilities.getFormat(DateUtilities.DATE_FULL)%>",
    shortTime:      "<%=DateUtilities.getFormat(DateUtilities.TIME_SHORT)%>"
    };
  Date.today="<%=I18n.get("TODAY")%>";

  Number.decimalSeparator = "<%=symbols.getDecimalSeparator()%>";
  Number.groupingSeparator = "<%=symbols.getGroupingSeparator()%>";
  Number.minusSign = "<%=symbols.getMinusSign()%>";
  Number.currencyFormat = "<%=NumberUtilities.DEFAULT_CURRENCY_FORMAT%>";



  var millisInWorkingDay =<%=CompanyCalendar.MILLIS_IN_WORKING_DAY%>;
  var workingDaysPerWeek =<%=CompanyCalendar.WORKING_DAYS_PER_WEEK%>;

  function isHoliday(date) {
    var friIsHoly =<%=!CompanyCalendar.FRIDAY_IS_WORKING_DAY?"true":false%>;
    var satIsHoly =<%=!CompanyCalendar.SATURDAY_IS_WORKING_DAY?"true":false%>;
    var sunIsHoly =<%=!CompanyCalendar.SUNDAY_IS_WORKING_DAY?"true":false%>;

    var pad = function (val) {
      val = "0" + val;
      return val.substr(val.length - 2);
    };

    var holidays = "<%="#"+cc.getHolyDays().serialize().replaceAll("\\$~\\$","#")+"#"%>";

    var ymd = "#" + date.getFullYear() + "_" + pad(date.getMonth() + 1) + "_" + pad(date.getDate()) + "#";
    var md = "#" + pad(date.getMonth() + 1) + "_" + pad(date.getDate()) + "#";
    var day = date.getDay();

    return  (day == 5 && friIsHoly) || (day == 6 && satIsHoly) || (day == 0 && sunIsHoly) || holidays.indexOf(ymd) > -1 || holidays.indexOf(md) > -1;
    }


  <%-- ---------------------  OBJECT USED FOR I18N CLIENT SIDE ----------------------------------------------%>
  var i18n = {
    YES:                 "<%=I18n.get("YES")%>",
    NO:                  "<%=I18n.get("NO")%>",
    FLD_CONFIRM_DELETE:  "<%=I18n.get("FLD_CONFIRM_DELETE")%>",
    INVALID_DATA:        "<%=I18n.get("INVALID_DATA")%>",
    ERROR_ON_FIELD:      "<%=I18n.get("ERROR_ON_FIELD")%>",
    OUT_OF_BOUDARIES:      "<%=I18n.get("OUT_OF_BOUDARIES")%>",
    CLOSE_ALL_CONTAINERS:"<%=I18n.get("CLOSE_ALL_CONTAINERS")%>",
    DO_YOU_CONFIRM:      "<%=I18n.get("DO_YOU_CONFIRM")%>",
    ERR_FIELD_MAX_SIZE_EXCEEDED:      "<%=I18n.get("ERR_FIELD_MAX_SIZE_EXCEEDED")%>",
    WEEK_SHORT:      "<%=I18n.get("WEEK_SHORT")%>",

    FILE_TYPE_NOT_ALLOWED:"<%=I18n.get("FILE_TYPE_NOT_ALLOWED")%>",
    FILE_UPLOAD_COMPLETED:"<%=I18n.get("FILE_UPLOAD_COMPLETED")%>",
    UPLOAD_MAX_SIZE_EXCEEDED:"<%=I18n.get("UPLOAD_MAX_SIZE_EXCEEDED")%>",
    ERROR_UPLOADING:"<%=I18n.get("ERROR_UPLOADING")%>",
    UPLOAD_ABORTED:"<%=I18n.get("UPLOAD_ABORTED")%>",
    DROP_HERE:"<%=I18n.get("DROP_HERE")%>",

    FORM_IS_CHANGED:     "<%=I18n.get("FORM_IS_CHANGED")%>",

    PIN_THIS_MENU: "<%=I18n.get("PIN_THIS_MENU")%>",
    UNPIN_THIS_MENU: "<%=I18n.get("UNPIN_THIS_MENU")%>",
    OPEN_THIS_MENU: "<%=I18n.get("OPEN_THIS_MENU")%>",
    CLOSE_THIS_MENU: "<%=I18n.get("CLOSE_THIS_MENU")%>",
    PROCEED: "<%=I18n.get("PROCEED")%>?",

    PREV: "<%=I18n.get("PREV")%>",
    NEXT: "<%=I18n.get("NEXT")%>",
    HINT_SKIP: "<%=I18n.get("HINT_SKIP")%>",

    WANT_TO_SAVE_FILTER: "<%=I18n.get("WANT_TO_SAVE_FILTER")%>",
    NEW_FILTER_NAME: "<%=I18n.get("NEW_FILTER_NAME")%>",
    SAVE: "<%=I18n.get("SAVE")%>",
    DELETE: "<%=I18n.get("DELETE")%>",
    HINT_SKIP: "<%=I18n.get("HINT_SKIP")%>",

    COMBO_NO_VALUES: "<%=I18n.get("COMBO_NO_VALUES")%>?",

    FILTER_UPDATED:"<%=I18n.get("FILTER_UPDATED")%>",
    FILTER_SAVED:"<%=I18n.get("FILTER_SAVED")%>"

    };


  <%if (false){%>
  </script>
  <%}%>
