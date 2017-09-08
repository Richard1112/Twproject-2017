<%@ page import=" com.twproject.resource.Person,
                  com.twproject.resource.Resource,
                  org.jblooming.designer.Designer,
                  org.jblooming.designer.DesignerField,
                  org.jblooming.designer.Detail,
                  org.jblooming.persistence.PersistenceHome,
                  org.jblooming.waf.PagePlugin,
                  org.jblooming.waf.PluginBricks,
                  org.jblooming.waf.SessionState,
                  org.jblooming.waf.constants.Commands,
                  org.jblooming.waf.html.core.JspIncluder, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.view.PageState,
                  java.util.Date, com.twproject.security.TeamworkPermissions"%><%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8"%><%!

  public class ResourceTargets extends PagePlugin {

    public boolean isVisibleInThisContext(PageState pagestate) {

      boolean ret = false;

      if (pagestate.getMainObject() != null ) {
        ret = "Twproject test form".equalsIgnoreCase(pagestate.getMainObject().getName());
      }

      return ret;
    }
  }
%><%

  if (JspIncluder.INITIALIZE.equals(request.getParameter(Commands.COMMAND))) {

    PluginBricks.getPagePluginInstance("RESOURCE_FORMS", new ResourceTargets(), request);

  } else {

  PageState pageState =  PageState.getCurrentPageState(request);

  Resource resource = (Resource) PersistenceHome.findByPrimaryKey(Resource.class,pageState.mainObjectId);

  Designer designer = (Designer)JspIncluderSupport.getCurrentInstance(request);
  designer.readOnly = !resource.hasPermissionFor(pageState.getLoggedOperator(), TeamworkPermissions.resource_canWrite);

  if (Designer.DRAW_FORM.equals(request.getAttribute(JspIncluder.ACTION))) {
    if (designer.fieldsConfig) {

      Detail detail = designer.addDetail("targets");
      detail.name = "targets";
      detail.label = pageState.getI18n("Targets for")+"&nbsp;"+resource.getDisplayName();
      DesignerField desc = new DesignerField(String.class.getName(), "description", pageState.getI18n("Description"), true, false, "target description");
      desc.fieldSize = 50;
      desc.rowsLength = 3;
      detail.add(desc);
      detail.add(new DesignerField(Date.class.getName(), "dueDate", pageState.getI18n("Due date"), false, false, ""));
      DesignerField perc = new DesignerField(Double.class.getName(), "percentile", pageState.getI18n("Percentile"), true, false, "0");
      perc.fieldSize = 5;
      detail.add(perc);

      designer.add(detail);


    } else {
     // layout

     designer.draw("targets",pageContext);

    }
  }
  }
%>