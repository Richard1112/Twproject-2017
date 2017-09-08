package org.jblooming.persistence.objectEditor;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.QueryHelper;
import org.jblooming.ontology.Identifiable;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.state.Form;
import org.jblooming.waf.html.button.ButtonSupport;

import javax.servlet.jsp.PageContext;
import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.Field;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 12-apr-2005 : 11.02.00
 */
public class ObjectEditor extends JspHelper {

  private Class mainObjectClass;

  public String title;

  public boolean idDrawn = true; // aggiunto per evitare, se voglio, che mi disegni l'id sull'editor

  /**
   * @deprecated use queryHelper
   */
  public String query;
  public QueryHelper queryHelper;

  public String defaultOrderBy = null;

  public boolean readOnly = false;
  public boolean canAdd=true;
  public boolean canDelete=true;
  public boolean canEdit=true;
  public boolean bulkActions=false;

  public Map<String, FieldFeature> displayFields = new LinkedHashMap();
  public Map<String, FieldFeature> editFields = new LinkedHashMap();
  public String mainHqlAlias;

  public static final String listUrl = "/commons/objectEditor/partObjectList.jsp";
  public static final String editUrl = "/commons/objectEditor/partObjectEditor.jsp";

  public ActionController objController;

  //public Class customizedDeletePreview;
  public boolean isMultipart=false;

  public boolean hideListButton = false;
  public boolean windowCloseOnSubmit = false;
  public boolean buttonLeft = false;
  public boolean showDuplicateButton = false;

  public List<ButtonSupport> additionalButtons = new ArrayList<ButtonSupport>();

  public Form form;


  private ObjectEditor(String title) {
    super();
    this.title = title;
    this.urlToInclude = listUrl;
  }

  public ObjectEditor(String title, Class claz, PageContext pageContext) {
    this(title);
    try {
      if (!(claz.newInstance() instanceof Identifiable)) {
        throw new PlatformRuntimeException("Identifiable required");
      }
      this.mainObjectClass = claz;
    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    }

    PageState pageState = PageState.getCurrentPageState(pageContext);
    PageSeed v = pageState.thisPage((HttpServletRequest) pageContext.getRequest());
    form = new Form(v);
  }

  public Class getMainObjectClass() {
    return mainObjectClass;
  }


  public void addDisplayField(String fieldName, String alias) {
    FieldFeature ff = new FieldFeature(fieldName, alias);
    displayFields.put(ff.propertyName, ff);
  }

  public void addEditField(String fieldName, String alias) {
    FieldFeature ff = new FieldFeature(fieldName, alias);
    editFields.put(ff.propertyName, ff);
  }

  public void addDisplayField(FieldFeature ff) {
    displayFields.put(ff.propertyName, ff);
  }

  public void addEditField(FieldFeature ff) {
    editFields.put(ff.propertyName, ff);
  }

  public void fillFieldFeatures(Identifiable i) {

    Map<String, Field> dif = ReflectionUtilities.getDeclaredInheritedFields(mainObjectClass);
    for (Field f : dif.values()) {
      FieldFeature ff = new FieldFeature(f.getName(), StringUtilities.deCamel(f.getName()));
      addDisplayField(ff);
      addEditField(ff);
    }
  }


}
