package org.jblooming.designer;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Currency;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.ServletException;
import javax.servlet.jsp.PageContext;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.LookupSupport;
import org.jblooming.ontology.Node;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.security.License;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CodeValue;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.utilities.ReflectionUtilities;
import org.jblooming.waf.ActionUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.html.core.HtmlBootstrap;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.input.SQLCombo;
import org.jblooming.waf.html.input.SmartCombo;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.view.RestState;

import com.twproject.task.TaskCustomerFieldRelation;

import bsh.Interpreter;
import net.sf.json.JSONObject;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class DesignerField {

  public String kind;
  public String name;
  public String label;
  public String innerLabel;
  public String initialValue;

  public int cardinality = 1;

  public int maxLength = 0; //valore che in alcuni casi corrisponde alla size su db

  public int fieldSize = 20;
  public int rowsLength = 1;

  public boolean required = false;
  public boolean readOnly = false;


  public CodeValueList cvl;
  public SmartCombo smartCombo;
  public SQLCombo sqlCombo;

  public String urlFileStorage_urlToInclude;

  public JspHelper jspHelper;

  public boolean noSortable = false;
  /**
   * @deprecated
   */
  @Deprecated
public String mask = null;

  public String separator = null;

  public String format;
  // script allows to add various features like style or javascript
  // on the DesignerField visualization
  public String script;

  public String additionalScript;
  /**
   * method used for callback when displaying field content
   */
  public String toStringCallbackMethod;

  public PageSeed pageSeed;

  public boolean displayAsCombo = false;
  public boolean putLabelFirst = true;
  public boolean classic = false;

  public int minimumFractionDigits = -1;

  /**
   * if false the filed is not used for searching
   */
  public boolean usedForSearch = false;
  public boolean usedComboForSearch = false;
  public boolean useEmptyForAll = false;

  public boolean autoSize = false;

  public boolean exportable = false;

  public boolean preserveOldValue = true;

  public static final String DESIGNER_FIELD_SEPARATOR = "_";

  public DesignerField(String kind, String fieldName, String label, boolean required, boolean readOnly, String initialValue) {
    this.kind = kind;
    this.name = fieldName;
    this.label = label;
    this.required = required;
    this.readOnly = readOnly;
    this.initialValue = initialValue;
  }

  public DesignerField(String kind, String fieldName, String label) {
    this(kind, fieldName, label, false, false, null);
  }

  public DesignerField() {

  }

  public void toHtml(PageContext pageContext) {
    new Drawer(this).toHtml(pageContext);
  }


  public class Drawer extends JspHelper implements HtmlBootstrap {

    public DesignerField designerField;

    public Drawer(DesignerField fd) {
      super();
      this.designerField = fd;
      urlToInclude = "/commons/layout/designer/partDesignerField.jsp";
    }

    @Override
	public String getDiscriminator() {
      return Drawer.class.getName();
    }

    @Override
	public boolean validate(PageState pageState) throws IOException, ServletException {
      return true;
    }
  }

  public static DesignerField getIdentifiableInstance(String fieldName, String label, Class<? extends Identifiable> identifiableClass, String[] comboDisplayProperties) {
    DesignerField ff = new DesignerField(SmartCombo.class.getName(), fieldName, label);

    try {
      Object o = identifiableClass.newInstance();

      if (o instanceof Identifiable) {

        String hql = "select p.id";
        for (String cdp : comboDisplayProperties) {
          hql += ", p." + cdp;
        }
        hql += " from " + identifiableClass.getName() + " as p";
        String whereForFiltering = "where p." + comboDisplayProperties[0] + " like :" + SmartCombo.FILTER_PARAM_NAME + " order by p." + comboDisplayProperties[0];
        String whereForId = "where p.id = :" + SmartCombo.FILTER_PARAM_NAME;
        SmartCombo lookup = new SmartCombo(fieldName, hql, whereForFiltering, whereForId);
        lookup.fieldSize = ff.fieldSize;
        ff.smartCombo = lookup;

      } else
        throw new PlatformRuntimeException("DesignerField accepts Identifiable extensions only");

    } catch (InstantiationException e) {
      throw new PlatformRuntimeException(e);
    } catch (IllegalAccessException e) {
      throw new PlatformRuntimeException(e);
    }
    return ff;
  }

  public static DesignerField getSQLComboInstance(String fieldName, String label, String sqlSelect, String whereForFiltering, String whereForId) {
    SQLCombo sqlCombo = new SQLCombo(fieldName, sqlSelect, whereForFiltering, whereForId);
    DesignerField ret = new DesignerField(SQLCombo.class.getName(), fieldName, label);
    ret.sqlCombo = sqlCombo;

    return ret;
  }

  public static DesignerField getSimpleSQLComboInstance(String fieldName, String label, String tableName, String idColumnName, String descriptionColumnName) {
    SQLCombo sqlCombo = SQLCombo.getSimpleSQLCombo(fieldName, tableName, idColumnName, descriptionColumnName);
    DesignerField ret = new DesignerField(SQLCombo.class.getName(), fieldName, label);
    ret.sqlCombo = sqlCombo;

    return ret;
  }


  /**
   * Da usare per avere il valore "decodificato" pronto per essere stampato
   * @param value
   * @return
   */
  public String toDisplayString(String value) {
    String ret = "";

    if (!JSP.ex(value))
      return ret;

    try {
      Class type = Class.forName(kind);
      List classes = ReflectionUtilities.getInheritedClasses(type);

      //  -------------- SMART COMBO
      if (SmartCombo.class.getName().equals(this.kind) && smartCombo != null) {
        ret = smartCombo.getTextValue(value);


        // ------------------- SQLCOMBO
      } else if (SQLCombo.class.getName().equals(this.kind) && sqlCombo != null) {
        ret = sqlCombo.getTextValue(value);


        //---------------   DATE
      } else if (Date.class.getName().equals(this.kind)) {
        if (JSP.ex(value)) {
          Date d;
          try {
            d = DateUtilities.dateFromString(value, "yyyy-MM-dd-HH-mm-ss");
          } catch (ParseException e) {
            throw new PlatformRuntimeException("The serialized date format has been changed.", e);
          }
          ret = DateUtilities.dateToString(d);
        }


        //------------------------- DOUBLE
      } else if (Double.class.getName().equals(this.kind)) {
        if (JSP.ex(value)) {

          //read the string from the db and parse it using US locale, as db data is always in such locale
          NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
          DecimalFormat dFormat = (DecimalFormat) nf;
          try {

            double d = dFormat.parse(value).doubleValue();

            //now get the string to display and format it to configuered locale, which is static in system and hence used in NumberUtilities.decimal
            ret = NumberUtilities.decimalNoGrouping(d, this.getDecimalPlaces());

          } catch (ParseException e) {
            throw new PlatformRuntimeException(e);
          }
        }


        // ---------------------- CURRENCY
      } else if (Currency.class.getName().equals(this.kind)) {
        if (JSP.ex(value)) {
          ret = JSP.currency(Double.parseDouble(value));
        }


        //----------------- LOOKUPSUPPORT
      } else if (classes.contains(LookupSupport.class)) {
        LookupSupport ls = (LookupSupport) PersistenceHome.findByPrimaryKey(type, value);
        if (ls != null)
          ret = ls.getDescription();

        //----------------- IDENTIFIABLE
      } else if (classes.contains(Identifiable.class)) {
        Identifiable ls = PersistenceHome.findByPrimaryKey(type, value);
        if (ls != null)
          ret = ls.getDisplayName();

        //----------------- PERSISTENTFILE
      } else if (classes.contains(PersistentFile.class)) {
        PersistentFile pf = PersistentFile.deserialize(value);
        if (pf != null) {
          ret=pf.getDownloadOrViewLink().getHtml();
        }

        //-------------------------- BOOLEAN
      } else if (type.equals(Boolean.class) || "boolean".equals(type.toString())) {
        if (Fields.TRUE.equals(value))
          ret = I18n.get("TRUE");
        else if (Fields.FALSE.equals(value))
          ret = I18n.get("FALSE");


        //-------------------------- CODEVALUE
      } else if (type.equals(CodeValue.class)) {
        if (cvl != null && cvl.keySet().contains(value)) {
          ret = cvl.get(value);
        }

      } else {
        ret = JSP.w(value);
      }
    } catch (Throwable t) {
      Tracer.platformLogger.error(t);
    }
    return ret;
  }


  public String getValueForInputField(String value) {

    String ret = null;
    if (Date.class.getName().equals(this.kind)) {
      if (JSP.ex(value)) {
        Date d;
        try {
          d = DateUtilities.dateFromString(value, "yyyy-MM-dd-HH-mm-ss");
        } catch (ParseException e) {
          throw new PlatformRuntimeException("The serialized date format has been changed.", e);
        }
        ret = DateUtilities.dateToString(d);
      }
    } else if (Double.class.getName().equals(this.kind)) {
      if (JSP.ex(value)) {

        //read the string from the db and parse it using US locale, as db data is always in such locale
        NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat dFormat = (DecimalFormat) nf;
        try {
          double d = dFormat.parse(value).doubleValue();

          //now get the string to display and format it to configuered locale, which is static in system and hence used in NumberUtilities.decimal
          ret = NumberUtilities.decimalNoGrouping(d, this.getDecimalPlaces());

        } catch (ParseException e) {
          throw new PlatformRuntimeException(e);
        }

        /*dFormat.applyPattern(this.format == null ? "#.000" : this.format);
        dFormat.setMinimumFractionDigits(this.minimumFractionDigits);
        ret = dFormat.format(Double.parseDouble(value));         // -1234.567000 */
      }

    } else if (Currency.class.getName().equals(this.kind)) {
      if (JSP.ex(value)) {
        ret = JSP.currency(Double.parseDouble(value));
      }
    } else {
      ret = JSP.w(value);
    }
    return ret;
  }

  public int getDecimalPlaces() {
    return this.minimumFractionDigits > -1 ? this.minimumFractionDigits : NumberUtilities.DEFAULT_DECIMAL_PLACES;
  }


  public static DesignerField getCustomFieldInstance(String customFieldDefinitionPrefix, int customFieldIndex, Identifiable targetObject, boolean readonly, boolean isViewMode, boolean isSearch, RestState restState) {
    return getCustomFieldInstance(customFieldDefinitionPrefix, "customField", customFieldIndex, targetObject, readonly, isViewMode, isSearch, restState);
  }

  public static DesignerField getCustomFieldInstance(String customFieldDefinitionPrefix, String customFieldPrefix, int customFieldIndex, Identifiable targetObject, boolean readonly, boolean isViewMode, boolean isSearch, RestState restState) {

    DesignerField ret = null;
    try {

      String fieldName = customFieldDefinitionPrefix + customFieldIndex;
      if (I18n.isActive(fieldName)) {
        String tcfName1 = I18n.get(fieldName);
        boolean rootOnly = tcfName1.toLowerCase().matches(".*\\s*,\\s*rootonly.*");
        boolean childOnly = tcfName1.toLowerCase().matches(".*\\s*,\\s*childonly.*");
        boolean required = tcfName1.toLowerCase().matches(".*\\s*,\\s*required.*");

        //clean the string
        if (rootOnly)
          tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*rootonly", "");
        if (childOnly)
          tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*childonly", "");
        if (required)
          tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*required", "");

        //se siamo in search non può essere required
        required = required && !isSearch;


        //node data management
        if (targetObject != null && targetObject instanceof Node) {
          if (rootOnly && ((Node) targetObject).getParentNode() != null)
            return ret;
          else if (childOnly && ((Node) targetObject).getParentNode() == null) {
            return ret;
          }
        }

        String[] settings = tcfName1.split(",");
        int fieldSize = 20;
        try {
          fieldSize = settings.length > 1 ? (new Integer(settings[1].trim())) : 20;
        } catch (Throwable e) {
        }


        String label = I18n.get(JSP.w(settings[0]));

        Class classx = String.class; // di default sono tutte String
        JSONObject json = null;

        Pattern regex = Pattern.compile("(\\{(.|\\s)*\\})", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);

        if (settings.length > 2) {
          try {
            Matcher regexMatcher = regex.matcher(tcfName1);
            if (regexMatcher.find()) {
              json = JSONObject.fromObject(regexMatcher.group(1));
            }
          } catch (Throwable t) {
          }


          //si recupera la classe, che c'è, è sempre in posizione 2
          try {
            classx = Class.forName(settings[2].trim());
          } catch (Throwable t) {
          }

          try {
            if (json != null) {

              // ------------------------------- visibleIf -------------------------------
              if (targetObject != null && json.has("visibleIf")) {
                //define bsh interpreter
                Interpreter bsh = new Interpreter();
                bsh.getNameSpace().importClass("org.jblooming.utilities.JSP");
                bsh.getNameSpace().importClass("org.jblooming.utilities.DateUtilities");

                Object myObject = targetObject;
                // fill bsh environment with value
                bsh.set("gen", myObject);
                String objClass = ReflectionUtilities.deProxy(myObject.getClass().getName());
                bsh.eval("obj=(" + objClass + ")gen; ");
                Object result;
                try {
                  if (!((Boolean) bsh.eval(json.getString("visibleIf"))))
                    return ret;
                } catch (Throwable t) {
                  Tracer.platformLogger.error("Invalid filter on custom field \"" + (customFieldDefinitionPrefix + customFieldIndex) + "\": \"" + json.getString("visibleIf") + "\"");
                }
              }


              // ------------------------------- CODE VALUE LIST -------------------------------
              //case code value list. Es.: genere,25,{values:{"m":"maschio","f":"femmina"} ,displayAsCombo:true}  for a combo
              //                      Es.: genere,2,{values:{m:"maschio",f:"femmina"}}   for radio
              if (json.has("values")) {
                ret = new DesignerField(CodeValue.class.getName(), fieldName, label, required, readonly, "");
                ret.cvl = CodeValueList.getJSONInstance(json.getJSONObject("values"));
                if (json.has("displayAsCombo")) {
                  ret.displayAsCombo = json.getBoolean("displayAsCombo");
                }


                // ----------------------------------- SQL COMBO ---------------------------------------------
                // E. G.: external db field,25,{ query:{ ... } }
              } else if (json.has("query")) {
                JSONObject query = json.getJSONObject("query");

                // simple case:  uses tableName, idColumnName, descriptionColumnName
                // E.G.:  query:{ tableName:"olpl_operator", idColumnName:"id", descriptionColumnName:"loginname" }
                if (query.has("tableName") && query.has("idColumnName") && query.has("descriptionColumnName")) {
                  ret = DesignerField.getSimpleSQLComboInstance(fieldName, label, query.getString("tableName"), query.getString("idColumnName"), query.getString("descriptionColumnName"));

                  // complete case:  uses query
                  // E.G.:  {query: {
                  //          select:"select id,concat_ws(' ',name,surname) from olpl_operator",
                  //          whereForFiltering:"where concat_ws(' ',name,surname) like ? order by name,surname",
                  //          whereForId:"where id=?"
                  //        }
                } else if (query.has("select") && query.has("whereForFiltering") && query.has("whereForId")) {
                  ret = DesignerField.getSQLComboInstance(fieldName, label, query.getString("select"), query.getString("whereForFiltering"), query.getString("whereForId"));
                }


                //support for external connection
                //    E.G.: connection:{
                //            driver:"com.mysql.jdbc.Driver",
                //            url:"jdbc:mysql://192.168.0.90/tw5L",
                //            user:"root",
                //            password:"root"
                //          }
                if (json.has("connection")) {
                  JSONObject connection = json.getJSONObject("connection");
                  if (ret != null && ret.sqlCombo != null && connection.has("driver") && connection.has("url") && connection.has("user") && connection.has("password")) {
                    ret.sqlCombo.setConnectionData(connection.getString("driver"), connection.getString("url"), connection.getString("user"), connection.getString("password"));

                  } else {
                    Tracer.platformLogger.error("Invalid data for custom field: " + fieldName + "\n" + tcfName1);
                  }
                }

                if (json.has("searchAll") && ret != null && ret.sqlCombo != null) {
                  ret.sqlCombo.searchAll = json.getBoolean("searchAll");
                }

                ret.required = required;
              }

            }
          } catch (Throwable e) {
          }
        }

        //do not search for persistentFile!
        if (PersistentFile.class.equals(classx) && isSearch)
          return null;

        if (ret == null)
          ret = new DesignerField(classx.getName(), fieldName, label, required, readonly, "");


        ret.fieldSize = fieldSize;
        ret.separator = "";
        ret.exportable = isViewMode;
        ret.readOnly = readonly;


        //ret.maxLength=255;  // Customfields are mapped as plain text -> 255 chars

        //if size>100 use textarea
        if (fieldSize > 100) {
          if (!isSearch) {
            ret.rowsLength = 5;
            ret.fieldSize = 80;
            ret.autoSize = true;
          }
        }

        if (isSearch) //in search mode field size is max 30
          ret.fieldSize = ret.fieldSize > 30 ? 30 : ret.fieldSize;

        //make
        if (targetObject != null && restState != null && (restState.getEntry(fieldName).name == null || isViewMode)) {
          String value = (String) (ReflectionUtilities.getFieldValue(customFieldPrefix + customFieldIndex, targetObject));
          restState.addClientEntry(fieldName, ret.getValueForInputField(value));
        }

      }
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }

    return ret;
  }


  private String getSerializedData(RestState pagestate) throws PersistenceException {
    DesignerData dd = new DesignerData();
    dd.putValue(pagestate, this, this.name, this.name);
    return dd.getValueMap().get(this.name);
  }


  public static void saveCustomFields(String fieldDefinitionPrefix, int numOfFields, Identifiable mainObject, RestState pageState) throws PersistenceException {
    saveCustomFields(fieldDefinitionPrefix, "customField", numOfFields, mainObject, pageState);
  }

  public static void saveCustomFields(String fieldDefinitionPrefix, String fieldPrefix, int numOfFields, Identifiable mainObject, RestState pageState) throws PersistenceException {
    for (int i = 1; i <= numOfFields && License.assertLevel(30); i++) {
      ClientEntry entry = pageState.getEntry(fieldDefinitionPrefix + i);
      if (entry.name != null) {
        DesignerField customFieldInstance = DesignerField.getCustomFieldInstance(fieldDefinitionPrefix, fieldPrefix, i, null, false, false, false, null);
        ClientEntry ce = null;
        // il custom field è null quando non è definito nelle label ma usato per salvare dati esterni strani eg xenesys
        if (customFieldInstance == null) {
          ce = entry;
        } else {
          ce = new ClientEntry(fieldDefinitionPrefix + i, customFieldInstance.getSerializedData(pageState));// è uno schifo ma ha il suo senso
        }
        ActionUtilities.setString(ce, mainObject, fieldPrefix + i);
      }
    }
  }


  public static boolean queryCustomFields(String customFieldDefinitionPrefix, int numOfFields, String objectAlias, QueryHelper qhelp, RestState pageState) throws PersistenceException {
    return queryCustomFields(customFieldDefinitionPrefix, "customField", numOfFields, objectAlias, qhelp, pageState);
  }

  public static boolean queryCustomFields(String customFieldDefinitionPrefix, String customFieldPrefix, int numOfFields, String objectAlias, QueryHelper qhelp, RestState pageState) throws PersistenceException {

    boolean somethingSearched = false;
    for (int i = 1; i <= numOfFields && License.assertLevel(30); i++) {
      ClientEntry entry = pageState.getEntry(customFieldDefinitionPrefix + i);
      if (entry.name != null) {
        DesignerField df = DesignerField.getCustomFieldInstance(customFieldDefinitionPrefix, i, null, false, false, false, null);
        if (df != null) {
          if (Double.class.getName().equals(df.kind) ||
            Long.class.getName().equals(df.kind) ||
            Integer.class.getName().equals(df.kind) ||
            String.class.getName().equals(df.kind)) {
            somethingSearched = somethingSearched | ActionUtilities.addQBEClause(customFieldDefinitionPrefix + i, objectAlias + "." + customFieldPrefix + i, customFieldPrefix + i, qhelp, QueryHelper.TYPE_CHAR, pageState);
          } else if (Date.class.getName().equals(df.kind)) {  //todo gestire caso Date
            somethingSearched = somethingSearched | ActionUtilities.addOQLClause(customFieldDefinitionPrefix + i, objectAlias + "." + customFieldPrefix + i, customFieldPrefix + i, qhelp, QueryHelper.TYPE_CHAR, pageState);

          } else {
            somethingSearched = somethingSearched | ActionUtilities.addOQLClause(customFieldDefinitionPrefix + i, objectAlias + "." + customFieldPrefix + i, customFieldPrefix + i, qhelp, QueryHelper.TYPE_CHAR, pageState);
          }
        }
      }
    }
    return somethingSearched;
  }

  public static boolean hasCustomField(String customFieldDefinitionPrefix, int numOfFields) {
    boolean hasCustomField = false;
    int i = 1;
    while (!hasCustomField && i <= numOfFields && License.assertLevel(30)) {
      hasCustomField = I18n.isActive(customFieldDefinitionPrefix + i);
      i++;
    }
    return hasCustomField;
  }

	public static DesignerField getCustomFieldInstance(TaskCustomerFieldRelation rl, Identifiable targetObject,
			boolean readonly, boolean isViewMode, boolean isSearch, RestState pageState) {
		DesignerField ret = null;
		try {
			String fieldName = "CUSTOMER_FILED" + rl.getId().toString();
			if (true) {
				String tcfName1 = rl.getField().getName();
				boolean rootOnly = tcfName1.toLowerCase().matches(".*\\s*,\\s*rootonly.*");
				boolean childOnly = tcfName1.toLowerCase().matches(".*\\s*,\\s*childonly.*");
				boolean required = tcfName1.toLowerCase().matches(".*\\s*,\\s*required.*");

				if (rootOnly)
					tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*rootonly", "");
				if (childOnly)
					tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*childonly", "");
				if (required) {
					tcfName1 = tcfName1.replaceAll("(?i)\\s*,\\s*required", "");
				}

				required = (required) && (!isSearch);

				if ((targetObject != null) && ((targetObject instanceof Node))) {
					if ((rootOnly) && (((Node) targetObject).getParentNode() != null))
						return ret;
					if ((childOnly) && (((Node) targetObject).getParentNode() == null)) {
						return ret;
					}
				}

				int fieldSize = 20;

				String label = tcfName1;

				Class classx = String.class;

				if ((org.jblooming.ontology.PersistentFile.class.equals(classx)) && (isSearch)) {
					return null;
				}
				if (ret == null) {
					ret = new DesignerField(classx.getName(), fieldName, label, required, readonly, "");
				}

				ret.fieldSize = fieldSize;
				ret.separator = "";
				ret.exportable = isViewMode;
				ret.readOnly = readonly;
				ret.initialValue = rl.getValue();

				if ((fieldSize > 100) && (!isSearch)) {
					ret.rowsLength = 5;
					ret.fieldSize = 80;
					ret.autoSize = true;
				}

				if (isSearch) {
					fieldSize = (fieldSize > 30 ? 30 : fieldSize);
				}
			}
		} catch (Throwable e) {
			Tracer.platformLogger.error(e);
		}

		return ret;
	}
}
