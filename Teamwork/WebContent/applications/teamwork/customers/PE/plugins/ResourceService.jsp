<%@ page import=" com.twproject.resource.Person,com.twproject.resource.Company,
                 com.twproject.task.Task,
                 java.util.Map,
                 com.teamwork.expand.ExpandInfo,
                 org.jblooming.designer.Designer,
                 org.jblooming.designer.DesignerField,
                 org.jblooming.designer.Detail,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.CodeValue,
                 org.jblooming.utilities.CodeValueList,
                 org.jblooming.utilities.DateUtilities,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.PagePlugin,
                 org.jblooming.waf.PluginBricks,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.container.Container,
                 org.jblooming.waf.html.core.JspIncluder,
                 org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.PercentileDisplay,
                 org.jblooming.waf.view.PageState,java.text.SimpleDateFormat,java.util.Date,
                 java.util.Date, org.jblooming.waf.html.input.SQLCombo, com.twproject.security.TeamworkPermissions" %><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %><%!

  /**
   * This inner class is used by Twproject to know if this form applies to current context.
   * PagePlugin classes are loaded at startup (or by hand) in memory to be performant.
   *
   */
  public class PagePluginExt extends PagePlugin {
    public boolean isVisibleInThisContext(PageState pagestate) {
      boolean ret = false;
      if (pagestate.getMainObject() != null&& pagestate.getMainObject().getClass().equals(Company.class)) {
    	  if( pagestate.command.equals(Commands.ADD)){
    		  ret=false;
    	  }else{
    		  ret=true;
    	  }
    	 
        }
      return ret;
    }
  }
%><%
  /*
  * Each custom form is composed by two parts called in different application life-cycle.
  *
  * The first part is the initialization. This part is called at startup and injects PagePlugin class in the system.
  * The PagePluginExt.isVisibleInThisContext method is called every time Teamworks is creating links for plugins.
  *
  * The second part is the definition of the form. Definition is composed of two parts: form data definition and form html layout.
  */
  // #########################################################################  BEGIN INITIALIZE  #########################################################################
  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {
	PagePluginExt pext= new PagePluginExt();
	pext.setName("服务内容");
	pext.setDescription("服务内容");
    PluginBricks.getPagePluginInstance("RESOURCE_FORMS",pext, request);

    // #########################################################################  END INITIALIZE  #########################################################################
  } else if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
    // ------- recover page model and objects -----  BEGIN DO NOT MOFIFY --------------
    PageState pageState = PageState.getCurrentPageState(request);
    Task task = (Task) PersistenceHome.findByPrimaryKey(Task.class, pageState.mainObjectId);
    Designer designer = (Designer) JspIncluderSupport.getCurrentInstance(request);
    // ------- recover page model and objects -----  END DO NOT MOFIFY --------------

    // check security and set read_only modality
    designer.readOnly = !task.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.task_canWrite);

    // #########################################################################  BEGIN FORM DATA DEFINITION  #########################################################################
    if (designer.fieldsConfig) {

      // Master Detail example. You can add a detail to the form and then add field to detail.
      Detail detail = designer.addDetail("DETAIL");
      detail.label = "供应商服务内容";
      
//       DesignerField dfperson = new DesignerField(Person.class.getName(), "PERSON", "供应商", false, false, null);
//       dfperson.fieldSize = 40;
//       detail.add(dfperson);
      
    //  DesignerField dfitem = new DesignerField(String.class.getName(), "ITEM", "服务内容", false, false, "");
//       DesignerField dfitem = DesignerField.getSQLComboInstance("SQLCOMBO", "Supplier Service Content", "select id,loginName from olpl_operator", "where loginName like ? order by loginName", "where id = ?");
//       dfitem.label="服务内容";
//       dfitem.separator = "</td><td>";
//       detail.add(dfitem);
      
      DesignerField dfitem = new DesignerField(ExpandInfo.class.getName(), "ITEM", "服务内容", false, false, null);
      dfitem.fieldSize = 55;
      detail.add(dfitem);
      

      DesignerField dfNote = new DesignerField(String.class.getName(), "NOTES", "描述（限制1000个汉字）", false, false, "");
      dfNote.fieldSize = 40;
      dfNote.rowsLength = 5;
      dfNote.separator = "</td><td>";
      detail.add(dfNote);
      
      
      DesignerField dftask = new DesignerField(Double.class.getName(), "TOTALPRICE", "单价", false, false, "0.00");
      dftask.separator = "</td><td>";
      dftask.fieldSize = 4;
      detail.add(dftask);

      DesignerField dfdate = new DesignerField(Date.class.getName(), "DATE", "日期", false, false, new SimpleDateFormat("YYYY-MM-dd").format(new Date()));
      detail.add(dfdate);
     




      // #########################################################################  END FORM DATA DEFINITION  ###################################################################


    } else {
      // #########################################################################  BEGIN FORM LAYOUT DEFINITION  ###############################################################



      // you can extract data to enrich your form using data from current task.
      // In this case we will extract missing days from current task
      String daysMissing = pageState.getI18n("UNSPECIFIED");
      if (task.getSchedule() != null && task.getSchedule().getEndDate() != null) {
        if (task.getSchedule().getValidityEndTime() > new Date().getTime()) {
          long missing = task.getSchedule().getValidityEndTime() - new Date().getTime();
          daysMissing = DateUtilities.getMillisInDaysHoursMinutes(missing);
        } else
          daysMissing = "<b>" + pageState.getI18n("OVERDUE") + "</b>";
      }
%>
<%-- <h1><%=task.getDisplayName()%></h1> --%>
<%-- -------------------  BEGIN TASK DATA ----------------- You can use the task recovered before to display cue data   --%>

<%-- -------------------  END TASK DATA -----------------  --%>
  <%-- -------------------  BEGIN HTML GRID ----------------- --%>

    <table><tr><td><%designer.draw("DETAIL", pageContext);%></td></tr></table>

  <%-- -------------------  END HTML GRID ----------------- --%>

      <%
    }
   // #########################################################################  END FORM LAYOUT DEFINITION  ####################################################################
  }
%>