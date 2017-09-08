<%@ page import="
 com.twproject.security.TeamworkPermissions, com.twproject.task.Assignment, com.twproject.task.Task, com.twproject.task.TaskDependency, com.twproject.utilities.TeamworkComparators, org.apache.poi.hssf.usermodel.HSSFRow,
 org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.ExcelWriter, org.jblooming.utilities.StringUtilities, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState, java.util.List, java.util.ArrayList, org.apache.poi.hssf.usermodel.HSSFCellStyle, org.apache.poi.ss.usermodel.DataFormat"
%><%@ page pageEncoding="UTF-8"%><%
  PageState pageState = PageState.getCurrentPageState(request);
  Task rootTask = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
  rootTask.testPermission(pageState.getLoggedOperator(), TeamworkPermissions.task_canRead);

  String filenameEncoded = StringUtilities.normalize(rootTask.getName());
  response.setHeader("content-disposition", "attachment; filename=\"" + filenameEncoded + ".xls\"");
  response.setContentType("application/vnd.ms-excel");


  ServletOutputStream sout = response.getOutputStream();
  ExcelWriter excelWriter = new ExcelWriter(sout);

  excelWriter.printHeader(new String[]{
    I18n.get(""),
    I18n.get("TASK_CODE"),
    I18n.get("TASK_NAME"),
    I18n.get("STATUS"),
    I18n.get("MILESTONE"),
    I18n.get("START"),
    I18n.get("MILESTONE"),
    I18n.get("END"),
    I18n.get("DURATION"),
    I18n.get("PROGRESS"),
    I18n.get("DEPENDS"),
    I18n.get("ASSIGNMENTS")
  });

  int col=0;
  excelWriter.sheet.setColumnWidth(col++,10);
  excelWriter.sheet.setColumnWidth(col++,20);
  excelWriter.sheet.setColumnWidth(col++,50);
  excelWriter.sheet.setColumnWidth(col++,20);

  //get descendant correctly sorted
  List<Task> tasks = rootTask.getDescendants(new TeamworkComparators.TaskManualOrderComparator());

  DataFormat format = excelWriter.wb.createDataFormat();
  HSSFCellStyle oneDecimalStyle = excelWriter.wb.createCellStyle();
  oneDecimalStyle.setDataFormat(format.getFormat("0.0"));

  HSSFCellStyle styleCurrencyFormat = excelWriter.wb.createCellStyle();
  styleCurrencyFormat.setDataFormat((short)8);


  //add "root" on top
  tasks.add(0, rootTask);

  int rootLevel= rootTask.getDepth();

  int i=1;
  for (Task task : tasks) {

    List values= new ArrayList();
    values.add(i);
    values.add(task.getCode());
    values.add(task.getName());
    values.add(I18n.get(task.getStatus()));
    values.add(task.isStartIsMilestone()?"*":"");
    values.add(task.getSchedule().getStartDate());
    values.add(task.isEndIsMilestone()?"*":"");
    values.add(task.getSchedule().getEndDate());
    values.add(task.getDuration());
    values.add(task.getProgress());

    //add dependencies
    String depString = "";
    boolean hasExternalDep=false;

    for (TaskDependency td : task.getPreviouses()) {
      Task superior = td.getDepends();
      //retrieve superior index in list
      int pos = tasks.indexOf(superior);
      if (pos >= 0) {
        depString = depString + (depString.length() == 0 ? "" : ",") + (pos + 1) + (td.getLag() == 0 ? "" : ":" + td.getLag()); //dependencies are 1 based
      } else {
        hasExternalDep = true;
      }
    }

    for (TaskDependency td : task.getNexts()) {
      Task inferior = td.getTask();
      //retrieve inferior index in list
      int pos = tasks.indexOf(inferior);
      if (pos < 0) {
        hasExternalDep = true;
      }
    }

    values.add(depString);


    //Assignments
    for (Assignment ass : task.getAssignementsSortedByRole()) {
      values.add(ass.getDisplayNameWithResource());
    }

    HSSFCellStyle indentCellStyle = excelWriter.wb.createCellStyle();
    indentCellStyle.setIndention((short)(task.getDepth()-rootLevel));


    HSSFRow exclRow = excelWriter.println(values.toArray());
    exclRow.getCell(1).setCellStyle(indentCellStyle);
    exclRow.getCell(2).setCellStyle(indentCellStyle);
    exclRow.getCell(9).setCellStyle(oneDecimalStyle);

    i++;
  }

  excelWriter.generate();
  sout.close();

%>