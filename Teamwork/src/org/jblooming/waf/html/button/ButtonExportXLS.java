package org.jblooming.waf.html.button;

import net.sf.json.JSONObject;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.SerializedList;
import org.jblooming.ontology.Identifiable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageState;

import javax.servlet.jsp.PageContext;
import java.util.List;
import java.util.ArrayList;


public class ButtonExportXLS extends ButtonSubmit {

  String urlForExport = "/commons/tools/exportXLS.jsp";
  public String outputFileName = "list";
  public String outputFileExt = "xls";
  public String sheetName = "";
  public boolean debug;

  public SerializedList<String> propertiesToExport = new SerializedList();
  public SerializedList<String> filterFieldToMonitor = new SerializedList();
  public SerializedList<String> fieldLabels = new SerializedList();
  public SerializedList<String> importClasses = new SerializedList();
  public ActionController controller;
  public List<Identifiable> objectList = new ArrayList<Identifiable>();

  public String entityAlias = "obj";
  private String command;


  /**
   * @param form
   * @param controllerClass
   * @param command
   * @param propertyToExport is a list of property names of object contained in the Page resulting from controller
   *                         for instance if page contains a list of Operator you can use "name" "surname" or "anagraphicalData.address".
   *                         You can use also BSH espression. In this case the property MUST start with "BSH:" string like "BSH:obj.getDisplayName().toUpperCase()" and MUST refer to
   *                         the entity with "obj" name or by changing entityAlias property
   */

  public ButtonExportXLS(Form form, Class controllerClass, String command, String... propertyToExport) {
    super(form);
    this.setCommand(command);
    preserveFormStatus = true;
    for (String p : propertyToExport) {
      propertiesToExport.add(p);
    }
    try {
      controller = (ActionController) controllerClass.newInstance();
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
    this.debug = ApplicationState.platformConfiguration.development;
  }

  private ButtonExportXLS() {
    super(null);
  }

  /**
   * @param importClasses is used to bsh for utility
   */
  public void addImportClasses(String... importClasses) {
    for (String p : importClasses) {
      this.importClasses.add(p);
    }
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

  public void addImportClass(String importClass) {
    if (JSP.ex(importClass)) importClasses.add(importClass);
  }

  public void addFieldToExport(String propertyToExport) {
    addFieldToExport(propertyToExport, "");
  }

  public void addFieldToExport(String propertyToExport, String propertyLabel) {
    addFieldToExport(propertyToExport, propertyLabel, "");
  }

  /**
   * @param propertyToExport  is the name of object contained in the Page resulting from controller
   *                          for instance if page contains a list of Operator you can use "name" "surname" or "anagraphicalData.address".
   *                          You can use also BSH espression. In this case the property MUST start with "BSH:" string like "BSH:obj.getDisplayName().toUpperCase()" and MUST refer to
   *                          the entity with "obj" name or changing entityAlias property.
   * @param propertyLabel
   * @param filterFieldCeName
   */
  public void addFieldToExport(String propertyToExport, String propertyLabel, String filterFieldCeName) {
    if (JSP.ex(propertyToExport)) {
      propertiesToExport.add(propertyToExport);
      fieldLabels.add(I18n.get(JSP.w(propertyLabel)));
      filterFieldToMonitor.add(JSP.w(filterFieldCeName));
    }
  }

  public void addBSHFieldToExport(String propertyToExport) {
    addFieldToExport("BSH:" + propertyToExport);
  }

  public void addBSHFieldToExport(String propertyToExport, String propertyLabel) {
    addFieldToExport("BSH:" + propertyToExport, propertyLabel);
  }

  public void addBSHFieldToExport(String propertyToExport, String propertyLabel, String filterFieldCeName) {
    addFieldToExport("BSH:" + propertyToExport, propertyLabel, filterFieldCeName);
  }


  public void toHtml(PageContext pageContext) {
    PageState.getCurrentPageState(pageContext).sessionState.setAttribute(ButtonExportXLS.class.getName() + "_" + this.id, this);
    form.url.addClientEntry("BUTTON_ID", "");
    variationsFromForm.setCommand(command);
    variationsFromForm.href = ApplicationState.contextPath + urlForExport;
    variationsFromForm.addClientEntry("BUTTON_ID", this.id);

    super.toHtml(pageContext);
  }

  public void setCommand(String command) {
    this.command = command;
  }

  public void addFieldsToExport(JSONObject properties) {
    for (Object prop : properties.keySet()) {
      addBSHFieldToExport(properties.getString(prop + ""), prop + "");
    }

  }
}

