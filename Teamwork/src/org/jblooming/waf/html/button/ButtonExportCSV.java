package org.jblooming.waf.html.button;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.SerializedList;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.jsp.PageContext;

public class ButtonExportCSV extends ButtonSubmit {

  String urlForExport = "/commons/tools/exportCSV.jsp";
  public String outputFileName = "list";
  public String outputFileExt = "csv";
  public char delimiter;

  public SerializedList<String> propertiesToExport = new SerializedList();
  public SerializedList<String> filterFieldToMonitor = new SerializedList();
  public SerializedList<String> fieldLabels = new SerializedList();
  public ActionController controller;

  public String entityAlias="obj";
  private String command;


  /**
   *
   * @param form
   * @param controllerClass
   * @param command
   * @param propertyToExport is a list of property names of object contained in the Page resulting from controller
   *        for instance if page contains a list of Operator you can use "name" "surname" or "anagraphicalData.address".
   *        You can use also BSH espression. In this case the property MUST start with "BSH:" string like "BSH:obj.getDisplayName().toUpperCase()" and MUST refer to
   *        the entity with "obj" name or by changing entityAlias property
   *
   */

  public ButtonExportCSV(Form form, Class controllerClass, String command, String... propertyToExport) {
    super(form);
    this.command=command;
    preserveFormStatus = true;
    for (String p : propertyToExport) {
      propertiesToExport.add(p);
    }
    try {
      controller = (ActionController)controllerClass.newInstance();
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
  }

  private ButtonExportCSV() {
    super(null);
  }

  /**
   * @param fieldLabelss is used to extract filter description
   */
  public void addFieldlabels(String... fieldLabelss) {
    for (String p : fieldLabelss) {
      addFieldToExport(p);
    }
  }

  /**
   * @param filterField is used to extract filter description
   */
  public void addFilterField(String... filterField) {
    for (String p : filterField) {
      filterFieldToMonitor.add(p);
    }
  }


  public void addFieldToExport(String propertyToExport) {
    addFieldToExport(propertyToExport, "");
  }

  public void addFieldToExport(String propertyToExport, String propertyLabel) {
    addFieldToExport(propertyToExport, propertyLabel, "");
  }

  /**
   *
   * @param propertyToExport is the name of object contained in the Page resulting from controller
   *        for instance if page contains a list of Operator you can use "name" "surname" or "anagraphicalData.address".
   *        You can use also BSH espression. In this case the property MUST start with "BSH:" string like "BSH:obj.getDisplayName().toUpperCase()" and MUST refer to
   *        the entity with "obj" name or changing entityAlias property.
   * @param propertyLabel
   * @param filterFieldCeName
   */
  public void addFieldToExport(String propertyToExport, String propertyLabel, String filterFieldCeName) {
    if (JSP.ex(propertyToExport)) {
      propertiesToExport.add(propertyToExport);
      fieldLabels.add(JSP.w(propertyLabel));
      filterFieldToMonitor.add(JSP.w(filterFieldCeName));
    }
  }

  public void addBSHFieldToExport(String propertyToExport) {
    addFieldToExport("BSH:"+propertyToExport);
  }
  public void addBSHFieldToExport(String propertyToExport, String propertyLabel) {
    addFieldToExport("BSH:"+propertyToExport,propertyLabel);
  }

  public void addBSHFieldToExport(String propertyToExport, String propertyLabel, String filterFieldCeName) {
    addFieldToExport("BSH:"+propertyToExport,propertyLabel,filterFieldCeName);
  }


  public void toHtml(PageContext pageContext) {
    PageState.getCurrentPageState((HttpServletRequest) pageContext.getRequest()).sessionState.setAttribute(ButtonExportCSV.class.getName(),this);

    variationsFromForm.setCommand(command);
    variationsFromForm.href = ApplicationState.contextPath + urlForExport;    
    
    super.toHtml(pageContext);
  }

}


