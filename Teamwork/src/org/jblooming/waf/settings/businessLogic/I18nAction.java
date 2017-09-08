package org.jblooming.waf.settings.businessLogic;

import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.utilities.HttpUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.settings.Application;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.settings.I18nEntryPersistent;
import org.jblooming.waf.view.PageState;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.oql.OqlQuery;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.List;

public class I18nAction {
  public static final String SEPARATOR = "_-_";


  public void cmdList(PageState pageState) {

    if (pageState.getEntry("SEARCH_MISSING_IN_LANGUAGE").checkFieldValue() && !JSP.ex(pageState.getEntry(Fields.FORM_PREFIX + "APPLICATION"))) {
      pageState.getEntry(Fields.FORM_PREFIX + "APPLICATION").errorCode = pageState.getI18n("MISSING_IN_LANGUAGE_CHOOSE_APPLICATION");
      //throw new ActionException();
    }

  }


  public void cmdEdit(PageState pageState) {
    I18n i18nManager = ApplicationState.i18n;
    String code = pageState.getEntry(Fields.FORM_PREFIX + "code").stringValueNullIfEmpty();
    Set supportedLanguages = i18nManager.supportedLanguages;
    Collection applics = ApplicationState.platformConfiguration.applications.values();

    pageState.addClientEntry(Fields.FORM_PREFIX + "code", code);
    if (code != null) {
      for (Iterator iterator = applics.iterator(); iterator.hasNext();) {
        Application applic = (Application) iterator.next();
        String appname = applic.getName();
        for (Iterator iterator1 = supportedLanguages.iterator(); iterator1.hasNext();) {
          String lang = (String) iterator1.next();
          pageState.addClientEntry(Fields.FORM_PREFIX + SEPARATOR + appname + SEPARATOR + lang, I18n.getRawLabel(code, appname, lang));
        }
      }
    }
  }


  public void cmdSave(PageState pageState, HttpServletRequest request) {
    pageState.initializeEntries("table");
    I18n i18nManager = ApplicationState.i18n;
    String code = pageState.getEntry(Fields.FORM_PREFIX + "code").stringValueNullIfEmpty();
    if (code != null) {
      for (Iterator iterator = pageState.getClientEntries().getEntryKeys().iterator(); iterator.hasNext();) {
        String key = (String) iterator.next();
        if (key.startsWith(Fields.FORM_PREFIX + SEPARATOR)) {
          String appname = key.substring((Fields.FORM_PREFIX + SEPARATOR).length());
          String lang = appname.substring(appname.indexOf(SEPARATOR) + 3);
          appname = appname.substring(0, appname.indexOf(SEPARATOR));
          String value = pageState.getEntry(key).stringValueNullIfEmpty();
          saveEntryInMemory(code, appname, lang, value);
        }
      }

      //ifftussei in dev dump else suidddb
      if (ApplicationState.platformConfiguration.development) {
        try {
          cmdDump(pageState, request);
        } catch (IOException e) {
          throw new PlatformRuntimeException(e);
        }
      } else {
        // remove all code
        OqlQuery oql = new OqlQuery("delete from " + I18nEntryPersistent.class.getName() + " where code=:code");
        oql.getQuery().setString("code", code);
        oql.getQuery().executeUpdate();

        for (Iterator iterator = pageState.getClientEntries().getEntryKeys().iterator(); iterator.hasNext();) {
          String key = (String) iterator.next();
          if (key.startsWith(Fields.FORM_PREFIX + SEPARATOR)) {
            String appname = key.substring((Fields.FORM_PREFIX + SEPARATOR).length());
            String lang = appname.substring(appname.indexOf(SEPARATOR) + 3);
            appname = appname.substring(0, appname.indexOf(SEPARATOR));
            String value = pageState.getEntry(key).stringValueNullIfEmpty();

            if (value != null) {
              I18nEntryPersistent ent = new I18nEntryPersistent();
              ent.setCode(code);
              ent.setApplication(appname);
              ent.setLanguage(lang);
              ent.setValue(value);
              try {
                ent.store();
              } catch (StoreException e) {
                throw new PlatformRuntimeException(e);
              }
            } else {
              i18nManager.removeEntry(code, appname, lang);
            }
          }
        }
      }
    }
  }

  public void saveEntryInMemory(String code, String appname, String lang, String value) {
    if (value != null) {
      I18n.I18nEntry i18ne = I18n.addEntry(code, appname, lang, value);
      i18ne.setSuspect(false);
    } else {
      I18n.removeEntry(code, appname, lang);
    }
    I18n.dumpNeeded = true;
  }


  public void cmdDump(PageState pageState, HttpServletRequest request) throws IOException {
    I18n i18nManager = ApplicationState.i18n;
    Collection applics = ApplicationState.platformConfiguration.applications.values();
    for (Iterator iterator = applics.iterator(); iterator.hasNext();) {
      Application application = (Application) iterator.next();
      String root = HttpUtilities.getFileSystemRootPathForRequest(request);
      File file = new File(root + File.separator + application.getRootFolder() + File.separator + "settings" + File.separator + "i18n" + File.separator + application.getName() + ".i18n");
      I18n.persistI18nByApplicationAndLanguage(application, file);
    }
    i18nManager.dumpNeeded = false;
  }


  public void cmdReload() {
    I18n i18nManager = ApplicationState.i18n;
    i18nManager.supportedLanguages.clear();
    i18nManager.codeEntries.clear();
    Collection applics = ApplicationState.platformConfiguration.applications.values();
    for (Iterator iterator = applics.iterator(); iterator.hasNext();) {
      Application application = (Application) iterator.next();
      try {
        I18n.loadI18n(application);
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      }
    }

    // and now load from db if the case
    if (!ApplicationState.platformConfiguration.development) {
      OqlQuery oql = new OqlQuery("select entry from " + I18nEntryPersistent.class.getName() + " as entry");
      List<I18nEntryPersistent> ens = oql.getQuery().list();
      for (I18nEntryPersistent i18 : ens) {
        i18nManager.addEntry(i18.getCode(), i18.getApplication(), i18.getLanguage(), i18.getValue());
      }

    }
    I18n.dumpNeeded = false;
  }


  public void cmdChangeModality(PageState pageState) throws ActionException, PersistenceException {
    I18n i18nManager = ApplicationState.i18n;
    String selectedEdit = pageState.getEntry(Fields.FORM_PREFIX + "I18N_MODALITY").stringValueNullIfEmpty();
    int selectedLenientLevel = i18nManager.getLenient();
    try {
      selectedLenientLevel = pageState.getEntry(Fields.FORM_PREFIX + "I18N_LENIENT").intValue();
    } catch (ParseException e) {
    }
    i18nManager.setLenient(selectedLenientLevel);

    i18nManager.setI18nEditingOperatorId("");
    if (I18n.EDIT_STATUS_EDIT.equals(selectedEdit)) {
      i18nManager.setEditStatus(I18n.EDIT_STATUS_EDIT);
      i18nManager.setI18nEditingOperatorId(pageState.getLoggedOperator().getId()+"");
    } else if (I18n.EDIT_STATUS_APPEND.equals(selectedEdit)) {
      i18nManager.setEditStatus(I18n.EDIT_STATUS_APPEND);
    } else { // default as read
      i18nManager.setEditStatus(I18n.EDIT_STATUS_READ);
    }

    i18nManager.catchUsedLabels = pageState.getEntry("CATCHUSEDLABELS").checkFieldValue();

  }

  public void cmdNewLanguage(PageState pageState) {
    I18n i18nManager = ApplicationState.i18n;
    String newLanguage = pageState.getEntry(Fields.FORM_PREFIX + "ADD_NEW_LANGUAGE").stringValueNullIfEmpty();
    if (newLanguage != null) {
      i18nManager.supportedLanguages.add(newLanguage.toUpperCase());

      // test if the language is enabled: else add it
      String enaLang = ApplicationState.applicationSettings.get("ENABLED_LANGUAGES");

      if (JSP.ex(enaLang) && !enaLang.toUpperCase().contains(newLanguage.toUpperCase())) {
        ApplicationState.applicationSettings.put("ENABLED_LANGUAGES", enaLang + " " +newLanguage);

        //save application settings
        ApplicationState.dumpApplicationSettings();

      }

    }

  }

  public void cmdStoreLabel(PageState pageState) {
    String code = pageState.getEntry(Fields.FORM_PREFIX + "code").stringValueNullIfEmpty();
    String label = pageState.getEntry(Fields.FORM_PREFIX + "label").stringValueNullIfEmpty();
    String application = pageState.getEntry(Fields.FORM_PREFIX + "appl").stringValueNullIfEmpty();
    String language = SessionState.getLocale().getLanguage();
    I18n i18nManager = ApplicationState.i18n;
    i18nManager.addEntry(code, application, language, label);
  }

  public void cmdImportFromFile(PageState pageState) {

    Uploader.UploadHelper uh = Uploader.getHelper("I18N_FILE", pageState);
    if (uh != null) {
      File tmpFIle = uh.temporaryFile;
      try {
        I18n.loadI18n(tmpFIle);
      } catch (IOException e) {
        throw new PlatformRuntimeException(e);
      }
      pageState.removeEntry("I18N_FILE");
    } else
      pageState.getEntry("I18N_FILE").errorCode = "FILE_NOT_FOUND";
  }


  public void cmdRemoveLabel(PageState pageState) {
    String code = pageState.getEntry("CODE").stringValueNullIfEmpty();
    String appName = pageState.getEntry("APPNAME").stringValueNullIfEmpty();

    if (JSP.ex(code, appName)) {
      for (String lang : I18n.supportedLanguages) {
        I18n.removeEntry(code, appName, lang);
      }


      if (!ApplicationState.platformConfiguration.development) {
        // remove all code
        OqlQuery oql = new OqlQuery("delete from " + I18nEntryPersistent.class.getName() + " where code=:code and application=:appl");
        oql.getQuery().setString("code", code);
        oql.getQuery().setString("appl", appName);
        oql.getQuery().executeUpdate();
      }
    }

  }

  public void cmdSaveEnabledLanguages(PageState pageState) {
    String enaLang = pageState.getEntry("ENABLED_LANGUAGES").stringValueNullIfEmpty();

    if (JSP.ex(enaLang))
      ApplicationState.applicationSettings.put("ENABLED_LANGUAGES", enaLang);
    else
      ApplicationState.applicationSettings.remove("ENABLED_LANGUAGES");


    //save application settings
    ApplicationState.dumpApplicationSettings();

    // reload labels
    cmdReload();

  }
}
