package com.teamwork.expand;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.Bricks;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.button.ButtonSupport;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Company;
import com.twproject.resource.Resource;
import com.twproject.task.Task;

public class TaskServiceBricks extends Bricks {
	 public Task mainObject;
	 public TeamworkOperator logged;

	  public TaskServiceBricks(Task r) {
	    this.mainObject = r;
	  }
	  
	  
	  
	  public static SmartCombo getServiceTypeCombo(String fieldName, PageState pageState) {
		    String hql = "select p.id, p.description from " +TaskServiceType.class.getName() + " as p ";
		    String whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
		    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("SERVICE_TYPE");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";

		    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
		      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_TYPE")), pageState.pageFromRoot("task/service/taskServiceType.jsp"));
		      addTT.additionalCssClass = "small";
		      serviceType.addEntityButton = addTT;
		    }

		    return serviceType;
		  }

	  
	  
	  public static SmartCombo getServiceContentCombo(String fieldName, PageState pageState) {
		    String hql = "select p.id, p.description from " +TaskServiceContent.class.getName() + " as p ";
		    String whereForFiltering = "where p.description like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.description";
		    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("SERVICE_CONTENT");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";
		    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
		      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_CONTENT")), pageState.pageFromRoot("task/service/taskServiceContent.jsp"));
		      addTT.additionalCssClass = "small";
		      serviceType.addEntityButton = addTT;
		    }
		    return serviceType;
	 }
	  
	  
	  
	  public static SmartCombo getSupplierCombo(String fieldName, PageState pageState,String supplierType) {
		    //String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Resource.class.getName() + " as resource ";
		    String hql = "select  p.id, p.name from " + Company.class.getName() + " as p left join p.type a ";
		    //String hql = "select p.id, p.name from " +Company.class.getName() + " as p ";
		    String whereForFiltering = "where a.stringValue = '"+supplierType+"' and p.name like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.name";
		    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("SUPPLIER");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";

//		    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
//		      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_TYPE")), pageState.pageFromRoot("task/service/taskServiceType.jsp"));
//		      addTT.additionalCssClass = "small";
//		      serviceType.addEntityButton = addTT;
//		    }

		    return serviceType;
		  }
	  
	  
	  
	  public static SmartCombo getDepartCombo(String fieldName, PageState pageState,String supplierType) {
		    //String hql = "select resource.id, resource.name, coalesce(resource.code,' ') from " + Resource.class.getName() + " as resource ";
		    String hql = "select  p.id, p.name from " + Company.class.getName() + " as p left join p.type a ";
		    //String hql = "select p.id, p.name from " +Company.class.getName() + " as p ";
		    String whereForFiltering = "where a.stringValue = '"+supplierType+"' and p.name like :" + SmartCombo.FILTER_PARAM_NAME + " order by p.name";
		    String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
		    SmartCombo serviceType = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
		    serviceType.label = I18n.get("Department");
		    serviceType.fieldSize = 25;
		    serviceType.separator = "<br>";

//		    if (pageState.getLoggedOperator().hasPermissionAsAdmin()) {
//		      ButtonSupport addTT = ButtonLink.getBlackInstance(JSP.wHelp(I18n.get("ADD_TYPE")), pageState.pageFromRoot("task/service/taskServiceType.jsp"));
//		      addTT.additionalCssClass = "small";
//		      serviceType.addEntityButton = addTT;
//		    }

		    return serviceType;
		  }
}
