package org.jblooming.waf.settings;

import com.Ostermiller.util.BadDelimiterException;
import com.Ostermiller.util.CSVParser;
import com.Ostermiller.util.CSVPrinter;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CodeValueList;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.html.input.Combo;
import org.jblooming.waf.view.PageState;

import java.io.*;
import java.nio.charset.Charset;
import java.util.*;


public class I18n {

  // edit status
  public final static String EDIT_STATUS_EDIT = "EDIT";
  public final static String EDIT_STATUS_READ = "READ";
  public final static String EDIT_STATUS_APPEND = "APPEND";

  // lenient level
  public static final int LENIENT_APP_LANG = 4;
  public static final int LENIENT_APP = 3;
  public static final int LENIENT_LANG = 2;
  public static final int LENIENT_NONE = 1;


  public static SortedSet<String> supportedLanguages = new TreeSet<String>();

  public static TreeMap<String, I18nEntry> codeEntries = new TreeMap();

  protected static int lenient = LENIENT_APP_LANG;
  private static String editStatus = EDIT_STATUS_READ;

  private static Serializable i18nEditingOperatorId;

  public static boolean catchUsedLabels = false;


  // commands
  public static final String CMD_CHANGEMODALITY = "CMD_CHMDLT";
  public static final String CMD_RELOAD = "CMD_REL";
  public static final String CMD_DUMP = "CMD_DU";
  public static final String CMD_NEW_LANGUAGE = "CMD_NL";
  public static final String CMD_STORE_LABEL = "CMD_STLBL";
  public static final String CMD_NEW_ENTRY = "CMD_NEW_ENTRY";
  public static final String CMD_REMOVE_LABEL = "CMDRMLBL";

  public static boolean dumpNeeded = false;



  /**
   * @param code
   * @return the label related to the "code". Can be exact or the most similar in relation to the lenient level
   */
  public static String get(String code) {
    if (JSP.ex(code)) {
      SessionState ss = SessionState.getCurrentSessionState();

      Locale loc=SessionState.getLocale();

      String opid = "";
      if (ss != null){
        opid = ss.getOpid() + "";
      }

      String language = loc.getLanguage().toUpperCase();
      String application = ApplicationState.platformConfiguration.defaultApplication.getName();

      I18nEntry i18nEntry = I18n.codeEntries.get(code);
      String label = null;

      if (i18nEntry != null && !i18nEntry.isSuspect()) // the suspected one are omitted
        label = i18nEntry.getLabel(application, language);

      // handling for "append" mode
      if (label == null && EDIT_STATUS_APPEND.equals(I18n.getEditStatus())) {
        I18nEntry i18ne = I18n.addEntry(code, application, language, "missing_" + code);
        i18ne.setSuspect(true);
      }

      if (label == null)
        label = code;

      // EDIT MODE HANDLING && EDITING OPERATOR ONLY
      if (EDIT_STATUS_EDIT.equals(I18n.editStatus) && (opid).equals(I18n.getI18nEditingOperatorId()) && !code.startsWith("CUSTOM_")) {
        label = "<span i18n=" + code + ">" + label + "</span>";
      }

      return label;
    } else
      return "";
  }

  /**
   * all the occurrences of '%%' in the i18nzation of rootToLocalize will be replaced by param
   */
  public static String get(String rootToLocalize, String... params) {
    String translation = get(rootToLocalize);
    return StringUtilities.replaceParameters(translation, params);
  }


  /**
   * Rimpiazza $param1$ ....$paramN$  con i valori presenti nella mappa
   * @param rootToLocalize
   * @param params
   * @return
   */

  public static String get(String rootToLocalize, Map<String,String>params) {
    String template = get(rootToLocalize);

    if (JSP.ex((Map) params)) {
      for (String key : params.keySet()) {
        String value = params.get(key);
        if (!JSP.ex(value))
          value = "-";
        else
          value = get(value);
        template = StringUtilities.replaceAllNoRegex(template, "$" + key + "$", value);
      }
    }
    return template;
  }



  public static String getLabel(String code, String language) {
    return I18n.getLabel(code, ApplicationState.platformConfiguration.defaultApplication.getName(), language);
  }

  public static String getLabel(String code, String application, String language) {
    // transform two part language as en_GB to EN only
    if (language.indexOf("_") > 1)
      language = language.substring(0, language.indexOf("_"));

    String label = null;
    I18nEntry i18nEntry = I18n.codeEntries.get(code);
    if (i18nEntry != null)
      label = i18nEntry.getLabel(application, language.toUpperCase());
    if (label == null)
      label = code;
    return label;
  }

  /**
   * @param code
   * @param code
   * @param language
   * @return the exact matching label. null if not found
   */
  public static String getRawLabel(String code, String application, String language) {
    I18nEntry i18nEntry = I18n.codeEntries.get(code);
    String label = null;
    if (i18nEntry != null) {
      ApplicationEntry applicationEntry =  i18nEntry.applicationEntries.get(application);
      if (applicationEntry != null) {
        label = applicationEntry.entries.get(language);
      }
    }
    return label;
  }



  public static SortedMap<String, I18nEntry> getEntriesForApplication(String applicationName) {
    TreeMap<String, I18nEntry> efa = new TreeMap();
    for (String code : codeEntries.keySet()) {
      final I18nEntry i18nEntry = codeEntries.get(code);
      final ApplicationEntry applicationEntry = (ApplicationEntry) i18nEntry.applicationEntries.get(applicationName);
      if (applicationEntry != null)
        efa.put(code, i18nEntry);
    }
    return efa;
  }

  public static I18nEntry addEntry(String code, String application, String language, String value) {
    I18n.supportedLanguages.add(language.toUpperCase());
    I18nEntry ie = I18n.codeEntries.get(code);
    if (ie == null) {
      ie = new I18nEntry(code, application, language, value);
    } else {

      ApplicationEntry ae = ie.applicationEntries.get(application);
      if (ae == null)
        ae = new ApplicationEntry(application);
      ie.applicationEntries.put(application, ae);
      ae.addEntry(language, value);
    }
    I18n.codeEntries.put(code, ie);
    return ie;
  }

  public static void removeEntry(String code, String application, String language) {
    if (I18n.getRawLabel(code, application, language) != null) {
      I18nEntry i18nEntry =  I18n.codeEntries.get(code);
      ApplicationEntry ae = i18nEntry.applicationEntries.get(application);
      ae.entries.remove(language);
      if (ae.entries.size() <= 0)
        i18nEntry.applicationEntries.remove(application);
      if (i18nEntry.applicationEntries.size() <= 0)
        I18n.codeEntries.remove(code);
    }
  }


  public static void persistI18nByApplicationAndLanguage(Application application, File file) throws IOException {

    final String applicationName = application != null ? application.getName() : "";
    SortedMap i18nEntriesForApplication = I18n.getEntriesForApplication(applicationName);

    for (String language : I18n.supportedLanguages) {

      File fileForLang = new File(FileUtilities.getNameWithoutExt(file.getPath()) + "." + language + ".i18n");

      fileForLang.getParentFile().mkdirs();
      FileOutputStream fos = new FileOutputStream(fileForLang);

      Charset charset = Charset.forName("UTF-8");

      OutputStreamWriter osw = new OutputStreamWriter(fos, charset);
      CSVPrinter cvsp = new CSVPrinter(osw);

      try {
        cvsp.changeDelimiter('\t');
      } catch (BadDelimiterException e) {
        throw new PlatformRuntimeException(e);
      }

      if (i18nEntriesForApplication != null && i18nEntriesForApplication.size() > 0) {
        for (Iterator iterator = i18nEntriesForApplication.keySet().iterator(); iterator.hasNext();) {
          String code = (String) iterator.next();
          I18nEntry ie = (I18nEntry) i18nEntriesForApplication.get(code);
          ApplicationEntry ae = (ApplicationEntry) ie.applicationEntries.get(applicationName);
          if (ae != null) {
            String value = (String) ae.entries.get(language);
            if (value != null) {
              cvsp.print(applicationName);
              cvsp.print(ie.getCode());
              cvsp.print(language);
              cvsp.println(value);
            }
          }

        }
      }
      osw.close();
      fos.close();
    }

  }


  public static void loadI18n(File file) throws IOException {
    FileInputStream fos = new FileInputStream(file);
    InputStreamReader isr = new InputStreamReader(fos, Charset.forName("UTF-8"));
    CSVParser cvsr = new CSVParser(isr);
    try {
      cvsr.changeDelimiter('\t');
      cvsr.setEscapes("nrf", "\n\r\f");
    } catch (BadDelimiterException e) {
      throw new PlatformRuntimeException(e);
    }

    String enabledLang=ApplicationState.applicationSettings.get("ENABLED_LANGUAGES");
    if (JSP.ex(enabledLang))
      enabledLang=enabledLang.toUpperCase();

    String values[][] = cvsr.getAllValues();
    if (values != null) {
      for (int i = 0; i < values.length; i++) {
        String[] value = values[i];
        if (value == null)
          Tracer.platformLogger.warn("open lab platform - empty row " + i + " for file " + file.getName());
        else if (value.length != 4)
          Tracer.platformLogger.warn("open lab platform - incomplete row " + i + ": " + JSP.arrayToString(value, " , ") + " for file " + file.getName());
        else {
          // check if language is enabled
          if (JSP.ex(enabledLang)){
            if (enabledLang.contains(value[2].toUpperCase()))
              I18n.addEntry(value[1], value[0], value[2].toUpperCase(), value[3]);
          } else {
            I18n.addEntry(value[1], value[0], value[2].toUpperCase(), value[3]);
          }
        }
      }
    }
    isr.close();
    fos.close();
  }


  public static String getEditStatus() {
    return I18n.editStatus;
  }

  public static void setEditStatus(String editStatus) {
    I18n.editStatus = editStatus;
  }

  public static int getLenient() {
    return I18n.lenient;
  }

  public void setLenient(int lenient) {
    I18n.lenient = lenient;
  }

  public static void loadI18n(Application application) throws IOException {
    String i18nPath = application.getRootFolder() + File.separator + "settings" + File.separator + "i18n";
    String root = ApplicationState.webAppFileSystemRootPath;
    final String path = root + File.separator + i18nPath;
    File i18nFolder = new File(path);
    if (i18nFolder.exists()) {
      if (!i18nFolder.isDirectory())
        throw new PlatformRuntimeException(path + " must be directory");
      File[] files = i18nFolder.listFiles();
      for (int i = 0; i < files.length; i++) {
        File file = files[i];
        if (file.getName().toLowerCase().endsWith(".i18n"))
          I18n.loadI18n(file);
      }
      Tracer.platformLogger.info("open lab platform - " + application.getName().toLowerCase() + " i18n settings loaded ok");
    }
  }

  public static Serializable getI18nEditingOperatorId() {
    return I18n.i18nEditingOperatorId;
  }

  public void setI18nEditingOperatorId(Serializable i18nEditingOperatorId) {
    I18n.i18nEditingOperatorId = i18nEditingOperatorId;
  }

  /**
   * @param selectLang is a two char code
   */
  public static Locale getLocale(String selectLang) {

    Locale ret = Locale.UK;
    if (JSP.ex(selectLang)) { //
      try {
        if (selectLang.indexOf("_") != -1) {
          String[] l = selectLang.split("_");
          ret = new Locale(l[0], l[1]);
        } else {
          if ("en".equalsIgnoreCase(selectLang))
            ret = Locale.US;
          else if ("zh".equalsIgnoreCase(selectLang))
            ret = Locale.CHINA;
          else if ("br".equalsIgnoreCase(selectLang))
            ret = new Locale("pt", "BR");
          else
            ret = new Locale(selectLang.toLowerCase(), selectLang.toUpperCase());
        }
      } catch (Throwable e) {
        Tracer.platformLogger.error("Invalid lang properties:" + selectLang);
      }
    } else {
      Tracer.platformLogger.error("Invalid lang properties:" + selectLang);
    }
    return ret;
  }

  public static boolean isActive(String label) {
    return ! (label.equals(get(label)) || "no".equalsIgnoreCase(get(label)));
  }


  /**
   *
   */
  public static class I18nEntry {

    public String code;
    private boolean suspect = false;
    private boolean seen = false;

    public Map<String, ApplicationEntry> applicationEntries = new TreeMap<String, ApplicationEntry>();


    protected I18nEntry(String code, String application, String language, String value) {
      ApplicationEntry ae = applicationEntries.get(application);
      this.code = code;
      if (ae == null)
        ae = new ApplicationEntry(application);
      ae.addEntry(language, value);
      applicationEntries.put(application, ae);
    }


    protected ApplicationEntry getApplicationEntry(String application) {
      ApplicationEntry ret = null;
      String defaultApplication = Application.PLATFORM_APP_NAME;

      if (application == null) {
        application = defaultApplication;
      }
      ret = applicationEntries.get(application);
      if (ret == null) {
        if (lenient == LENIENT_APP || lenient == LENIENT_APP_LANG) {
          ret = ret = applicationEntries.get(defaultApplication);
          if (ret == null) {
            if (applicationEntries.size() > 0)
              ret = applicationEntries.get(applicationEntries.keySet().iterator().next());
          }
        } else {
          ret = new ApplicationEntry(application);
        }
      }
      return ret;
    }

    protected String getLabel(String application, String language) {
      if (I18n.catchUsedLabels)
        setSeen(true);
      return getApplicationEntry(application).getLabel(language);
    }

    public boolean matches(String textToFind) {
      return matches(textToFind, null);

    }

    public boolean matches(String textToFind, Set<String> languages) {
      boolean match = false;
      if (!JSP.ex(languages))
        languages = supportedLanguages;
      if (code.toLowerCase().indexOf(textToFind.toLowerCase()) >= 0) {
        match = true;
      } else {
        for (Iterator<ApplicationEntry> iterator = applicationEntries.values().iterator(); iterator.hasNext() && !match;) {
          ApplicationEntry ae = iterator.next();
          match = ae.matches(textToFind, languages);
        }

      }
      return match;
    }

    public boolean isSuspect() {
      return suspect;
    }

    public void setSuspect(boolean suspect) {
      this.suspect = suspect;
    }

    public boolean isSeen() {
      return seen;
    }

    public void setSeen(boolean seen) {
      this.seen = seen;
    }

    public String getCode() {
      return code;
    }

  }

  /**
   *
   */
  public static class ApplicationEntry {

    public String applicationName;

    public ApplicationEntry(String applicationName) {
      this.applicationName = applicationName;
    }

    public Map<String, String> entries = new TreeMap();


    protected void addEntry(String lang, String value) {
      entries.put(lang, value);
    }

    protected String getLabel(String language) {

      String ret = null;
      String defaultLanguage = ApplicationState.SYSTEM_LOCALE.getLanguage();
      if (defaultLanguage != null) {
        defaultLanguage = defaultLanguage.toUpperCase();
        if (defaultLanguage.indexOf("_") > 1)
          defaultLanguage = defaultLanguage.substring(0, defaultLanguage.indexOf("_"));
      }
      if (language == null) {
        language = defaultLanguage;
      }

      language = language.toUpperCase();
      ret = entries.get(language);
      if (ret == null && (lenient == LENIENT_LANG || lenient == LENIENT_APP_LANG)) {
        if (defaultLanguage != null)
          ret = entries.get(defaultLanguage);
        if (ret == null)
          ret = entries.get("EN");
        if (ret == null) {
          if (entries.size() > 0)
            ret = entries.get(entries.keySet().iterator().next());
        }

      }
      return ret;
    }

    public boolean matches(String textToFind) {
      return matches(textToFind, null);
    }

    public boolean matches(String textToFind, Set<String> languages) {
      boolean match = false;
      if (!JSP.ex(languages))
        languages = supportedLanguages;

      textToFind = textToFind.toLowerCase();
      /*for (Iterator<String> iterator = entries.values().iterator(); iterator.hasNext() && !match;) {
        String label = iterator.next();
        match = label.toLowerCase().indexOf(textToFind) >= 0 ;
      }*/
      for (String lang : languages) {
        String label = getLabel(lang);
        match = label.toLowerCase().indexOf(textToFind) >= 0;
        if (match)
          break;
      }

      return match;
    }
  }

  public static Combo getLocaleCombo(String fieldName, PageState pageState) {

    Set<String> supportedLanguages = I18n.supportedLanguages;
    String enabledLang=ApplicationState.applicationSettings.get("ENABLED_LANGUAGES");

    CodeValueList cvl = new CodeValueList();
    for (String lang : supportedLanguages) {
      // check if language is enabled
      if (JSP.ex(enabledLang)){
        if (enabledLang.contains(lang.toUpperCase()))
          cvl.add(getLocale(lang).toString(), pageState.getI18n(lang));
      } else {
        cvl.add(getLocale(lang).toString(), pageState.getI18n(lang));
      }
    }
    if (supportedLanguages.contains("EN"))
      cvl.add(Locale.UK + "", pageState.getI18n(Locale.UK + ""));

    Combo cb = new Combo(pageState.getI18n("SELECT_LANGUAGE_AND_LOCALE"), fieldName, "</td><td>", null, 30, null, cvl, "");
    return cb;

  }

  public static Combo getTimeZoneCombo(String fieldName, PageState pageState) {

    List<TimeZone> tzs= new ArrayList<TimeZone>();
    for (String tzid : TimeZone.getAvailableIDs()) {
      tzs.add(TimeZone.getTimeZone(tzid));
    }
    Collections.sort(tzs, new Comparator<TimeZone>() {
      public int compare(TimeZone o1, TimeZone o2) {
        return new Integer(o1.getRawOffset()).compareTo(o2.getRawOffset());
      }
    });

      CodeValueList cvl = new CodeValueList();
    for (TimeZone timeZone :tzs) {
      //cvl.add(timeZone.getID(),timeZone.getDisplayName(true,TimeZone.SHORT,SessionState.getLocale()));
      int offset = (int) (timeZone.getRawOffset() / 3600000);
      String offS=offset>0?" +"+offset:(offset<0?" "+offset:"");
      cvl.add(timeZone.getID(),"(GMT"+ offS +") "+timeZone.getID());
    }
    Combo cb = new Combo(I18n.get("SYSTEM_TIME_ZONE"), fieldName, "</td><td>", null, 40, null, cvl, "");
    return cb;

  }


}
