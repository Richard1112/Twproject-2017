package org.jblooming.designer;

import org.jblooming.waf.html.state.*;
import org.jblooming.waf.html.container.*;
import org.jblooming.waf.view.*;
import org.jblooming.waf.PageQuark;
import org.jblooming.*;
import org.jblooming.ontology.Identifiable;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;

import javax.servlet.*;
import javax.servlet.jsp.*;
import java.io.*;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class Designer extends PageQuark {

  public boolean fieldsConfig;

  public String designerNamePart;
  public Class<? extends Identifiable> referenceClass;
  public Serializable referenceId;

  public Form form;
  public ButtonBar buttonBar = new ButtonBar();

  public boolean readOnly = false;
  public boolean exportable = false;

  public LinkedHashMap<String, DesignerField> designerFields = new LinkedHashMap<String, DesignerField>();

  public static final String DRAW_FORM = "DRAW_FORM";
  public static final String DRAW_STEP = "DRAW_STEP";
  public static final String DRAW_BUTTONS = "DRAW_BUTTONS";

  private DesignerData designerData;

  public Designer(String urlToInclude, String designerNamePart, Class referenceClassName, Serializable referenceId) {
    super();
    this.urlToInclude = urlToInclude;
    this.referenceId = referenceId;
    this.referenceClass = referenceClassName;
    this.designerNamePart = designerNamePart;
  }

  public Detail addDetail(String name) {
    if (name==null || name.indexOf("_")>-1)
      throw new PlatformRuntimeException("Invalid name "+name+" for detail ('_' is not legal)");
    Detail d = new Detail();
    d.name = name;
    d.readOnly = readOnly;
    d.exportable=exportable;
    designerFields.put(name, d);
    return d;
  }

  public DesignerField add(DesignerField designerField) {
    designerField.readOnly =  designerField.readOnly||readOnly;
    designerField.exportable= designerField.exportable||exportable;
    designerFields.put(designerField.name, designerField);
    return designerField;
  }

  public void draw(String designerField, PageContext pageContext) {
    DesignerField df = designerFields.get(designerField);
    if (df!=null){
      df.readOnly=df.readOnly||readOnly;
      df.exportable=df.exportable||exportable;
      df.toHtml(pageContext);
    } else
      throw new PlatformRuntimeException("Invalid field \""+designerField+"\" in designer \""+ JSP.w(designerNamePart)+"\" reference class:"+JSP.w(referenceClass)+" reference id:"+JSP.w(referenceId)+"\"");
  }

  /**
   * @deprecated call drawForm
   */
  public void toHtml(PageContext pageContext) {
    throw new PlatformRuntimeException("call drawForm");
  }

  public void drawDesigner(Form htmlForm, PageContext pageContext) throws IOException, ServletException {
    this.form = htmlForm;
    drawForm(pageContext);
  }

  private void drawForm(PageContext pageContext) throws IOException, ServletException {
    pageContext.getRequest().setAttribute(Designer.DRAW_STEP, designerNamePart);
    pageContext.getRequest().setAttribute(ACTION, DRAW_FORM);
    super.toHtml(pageContext);
  }


  public void drawButtons(PageContext pageContext) throws IOException, ServletException {
    pageContext.getRequest().setAttribute(ACTION, DRAW_BUTTONS);
    urlToInclude = "/commons/layout/designer/partDesignerButtons.jsp";
    super.toHtml(pageContext);
  }

  public void configFields(PageContext pageContext) throws IOException, ServletException {
    fieldsConfig = true;
    drawForm(pageContext);
    fieldsConfig = false;
  }

  public boolean isRequired() {
    boolean result = false;
    for (DesignerField df : designerFields.values()) {
      if (df.required) {
        result = true;
        break;
      }
    }
    return result;
  }


  public boolean isFulfilled() {
    DesignerData designerData = getDesignerData();
    boolean result = true;
    for (DesignerField df : designerFields.values()) {
      if(df instanceof Detail) {
        Detail detail = (Detail) df;
        if(detail.detailDesignerFields != null && detail.detailDesignerFields.size()>0) {
          for (DesignerField df_D : detail.detailDesignerFields.values()) {
            if(df_D.required) {
              result = checkDetailFulFilled(designerData,detail);
              break;
            }
          }
          if(!result)
            break;
        }
      } else {
        String value = designerData.getValueMap().get(df.name);
        if (df.required && !(value != null && value.trim().length() > 0)) {
          result = false;
          break;
        }
      }
    }
    return result;
  }

  private boolean checkDetailFulFilled(DesignerData designerData,Detail detail) {
    if(designerData.getValueMap() != null && designerData.getValueMap().size()>0){
      HashSet<Integer> ids = new HashSet<Integer>();
      for (String key : designerData.getValueMap().keySet()) {
        if (key.contains(detail.name+"_")) {
          String id = key.substring(key.lastIndexOf("_")+1,key.length());
          try {
            ids.add(Integer.parseInt(id));
          } catch(NumberFormatException e) {
            Tracer.platformLogger.error(e);
          }
        }
      }
      if(ids != null && ids.size()>0) {
        for (int id : ids) {
          for (DesignerField df : detail.detailDesignerFields.values()) {
            String value = designerData.getValueMap().get(detail.name+"_"+df.name+"_"+id);
            if (df.required && !(value != null && value.trim().length() > 0)) {
              return false;
            }
          }
        }
      }
    }
    return true;
  }

  public DesignerData getDesignerData() {
    if (designerData == null)
      designerData = DesignerData.getInstance(designerNamePart, referenceId, referenceClass.getName());
    return designerData;
  }


  public ClientEntry getEntry(String fieldName, PageState pageState) {
    return pageState.getEntry(fieldName);
  }



  public String toDisplayString(String fieldName){
    String ret="";
    DesignerField df = designerFields.get(fieldName);
    if (df!=null)
      ret=df.toDisplayString(getDesignerData().getValueMap().get(fieldName));
    return ret;
  }

  /**
   * questo metodo deve essere usato per usare la definizione di un form per stampare i dati che vengono da un'altro designer data
   * @param fieldName
   * @param value
   * @return
   */
  public String toDisplayString(String fieldName,String value){
    String ret="";
    DesignerField df = designerFields.get(fieldName);
    if (df!=null)
      ret=df.toDisplayString(value);
    return ret;
  }
}


