package org.jblooming.security;

import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.view.ClientEntry;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.Date;
import java.util.Properties;

/**
 * Created by rbicchierai on 17/11/2016.
 */
public class License {

  private static License mainLicense=null;

  private final static String key="d7a5b1563cb11cd15dccbc6fd9ac006f";

  //dati sul file di licenza
  public String customerCode="";
  public int licenses=0;
  public int version=0;
  public Date expires=new Date();
  public int level=-1;
  public boolean mobile=false;
  public boolean enterprise=false;
  public boolean demo=true;
  public String licenseKey="------------------------------------------------------------------------------------";


  public boolean loadOk=false; //viene settata a true se il load da string o da file è stato completato
  public boolean newFormat=false; //specifica se la licenza è nel vecchio formato o nel nuovo wrapped


  public static License getLicense(){
    if (mainLicense==null){
      mainLicense= License.fromFile();
    }
    return mainLicense;
  }


  public static License fromFile() {
    License ret=new License();
    try {
      String appGloblaPath = ApplicationState.webAppFileSystemRootPath + File.separator + "WEB-INF" + File.separator + "lic.properties";
      String licTxt = FileUtilities.readTextFile(appGloblaPath);
      mainLicense=License.fromString(licTxt);
      ret=mainLicense;
    }  catch (Throwable t){
      Tracer.platformLogger.error("Error reading license file",t);
    }
    return ret;
  }


  public static License fromString(String licTxt) {
    License ret=new License();
    try {
      if (!licTxt.contains("custCode") || !licTxt.contains("licenses") ) {
        licTxt = StringUtilities.decryptBase64(licTxt, key);
        ret.newFormat=true;
      }

      Properties p = new Properties();
      p.load(new StringReader(licTxt));
      ret.customerCode = JSP.w(p.getProperty("custCode")).trim();
      ret.licenseKey = JSP.w(p.getProperty("license")).trim();
      ret.licenses=new ClientEntry("a",p.getProperty("licenses")).intValueNoErrorCodeNoExc();
      ret.expires = DateUtilities.dateFromString(JSP.w(p.getProperty("expires")).trim(), "dd/MM/yyyy");
      ret.version = new ClientEntry("a",p.getProperty("version")).intValueNoErrorCodeNoExc();
      ret.enterprise = new ClientEntry("a",(p.getProperty("enterprise"))).checkFieldValue();
      ret.mobile = new ClientEntry("a",p.getProperty("mobile")).checkFieldValue();
      ret.demo = new ClientEntry("a",(p.getProperty("demo"))).checkFieldValue();
      ret.level=new ClientEntry("a",p.getProperty("level")).intValueNoErrorCodeNoExc();

      ret.loadOk=true;
    } catch (Throwable t){
      Tracer.platformLogger.error("Error parsing license string",t);
    }

    return ret;
  }





  /**
   * save license on file wrapped and refresh "License.license" in memory
   */
  public void storeLicense() throws IOException {
    String licensePacked = getLicenseWrapped();
    String appGloblaPath = ApplicationState.webAppFileSystemRootPath + File.separator + "WEB-INF" + File.separator + "lic.properties";
    FileUtilities.writeToFile(appGloblaPath,licensePacked );

    newFormat=true; // dato che si salva sempre nel nuovo formato

    //rinfresca la licenza in memoria in modo da avere sempre la corrispondenza tra il file e l'Application
    License.mainLicense=this;

  }

  public String getLicenseWrapped() {
    String separator = "\n";
    String licenceProps = "# BEGIN TW" + version + " ACTIVATION KEY - COPY FROM HERE ON" + separator +
      "version=" + version + separator +
      "custCode=" + customerCode + separator +
      "expires=" + DateUtilities.dateToString(expires, "dd/MM/yyyy") + separator +
      "licenses=" + licenses + separator +
      "enterprise=" + (enterprise?"yes":"no")+separator +
      "mobile=" + (mobile?"yes":"no")+separator +
      "demo=" + (demo?"yes":"no")+separator  +
      (level>0?"level=" + level+separator :"") +
      "license=" + licenseKey + separator +
      "# END ACTIVATION KEY - END COPY";

    String _rawLic = StringUtilities.encryptBase64(licenceProps, key);
    int rowLen = 65;

    String licensePacked = "";
    while (_rawLic.length() > rowLen) {
      licensePacked += _rawLic.substring(0, rowLen - 1) + "\n";
      _rawLic = _rawLic.substring(rowLen - 1);
    }
    licensePacked += _rawLic + "\n";
    licensePacked="----- BEGIN TWPROJECT KEY -----\n" + licensePacked + "----- END TWPROJECT KEY -----\n";
    return licensePacked;
  }

  public static boolean assertLevel(int requiredLevel) {
    getLicense();
    return mainLicense.level==0||mainLicense.level>=requiredLevel;
  }

  public static String getLevelName(int level){
    return level==0?"FULL":level==5?"FREE":level==10?"BASIC":level==20?"ADVANCED":level==30?"ENTERPRISE":"TRIAL";
  }

  public String getLevelName(){
    return License.getLevelName(level);
  }
}

