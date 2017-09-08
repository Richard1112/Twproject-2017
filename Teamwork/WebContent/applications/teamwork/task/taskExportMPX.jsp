<%@ page import="
 com.twproject.exchange.msproject.businessLogic.ProjectImportExportControllerAction,
 com.twproject.resource.Person,
 com.twproject.task.Task,
 net.sf.mpxj.ProjectCalendar,
 net.sf.mpxj.ProjectFile,
 net.sf.mpxj.ProjectHeader,
 net.sf.mpxj.Resource,
 net.sf.mpxj.mpx.MPXWriter,
 org.jblooming.persistence.PersistenceHome,
 org.jblooming.utilities.HashTable,
 org.jblooming.utilities.HttpUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState, java.util.Locale, java.util.Map, org.jblooming.security.License"
%><%@ page pageEncoding="UTF-8"%><%
  if (License.assertLevel(20)) {
    PageState pageState = PageState.getCurrentPageState(request);

    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);


    ProjectFile msProjectFile = new ProjectFile();

    msProjectFile.setAutoTaskID(true);
    msProjectFile.setAutoTaskUniqueID(true);

    msProjectFile.setAutoResourceID(true);
    msProjectFile.setAutoResourceUniqueID(true);

    //
    // Configure the file to automatically generate outline levels
    // and outline numbers.
    //
    msProjectFile.setAutoOutlineLevel(true);
    msProjectFile.setAutoOutlineNumber(true);

    //
    // Configure the file to automatically generate WBS labels
    //
    msProjectFile.setAutoWBS(true);

    //
    // Configure the file to automatically generate identifiers for calendars
    // (not strictly necessary here, but required if generating MSPDI files)
    //
    msProjectFile.setAutoCalendarUniqueID(true);


    //
    // Add a default calendar called "Standard"
    //
    ProjectCalendar calendar = msProjectFile.addDefaultBaseCalendar();
    //msProjectFile.setDelimiter(';');
    //msProjectFile.setDecimalSeparator(',');
    //msProjectFile.setThousandsSeparator('.');

    //
    // Add a holiday to the calendar to demonstrate calendar exceptions
    //
  /*ProjectCalendarException exception = calendar.addCalendarException();
  exception.setFromDate(df.parse("13/03/2006"));
  exception.setToDate(df.parse("13/03/2006"));
  exception.setWorking(false); */

    //
    // Retrieve the project header and set the start date. Note Microsoft
    // Project appears to reset all task dates relative to this date, so this
    // date must match the start date of the earliest task for you to see
    // the expected results. If this value is not set, it will default to
    // today's date.
    //
    ProjectHeader header = msProjectFile.getProjectHeader();
    if (task.getSchedule() != null)
      header.setStartDate(task.getSchedule().getStartDate());

    Map<Person, Resource> personResource = new HashTable();
    ProjectImportExportControllerAction.exportNode(task, msProjectFile, 0, personResource);

    MPXWriter writer = new MPXWriter();
    if (JSP.ex(ApplicationState.getApplicationSetting("MPXJ_LOCALE"))) {
      writer.setLocale(new Locale(ApplicationState.getApplicationSetting("MPXJ_LOCALE")));
    }

    String filenameEncoded = StringUtilities.normalize(task.getName());

    response.setHeader("content-disposition", "inline; filename=" + filenameEncoded + ".mpx");
    response.setContentType(HttpUtilities.getContentType("mpx"));

    writer.write(msProjectFile, response.getOutputStream());
  }
%>