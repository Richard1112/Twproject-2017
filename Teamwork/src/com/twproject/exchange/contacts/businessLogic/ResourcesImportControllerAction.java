package com.twproject.exchange.contacts.businessLogic;

import com.Ostermiller.util.CSVParser;
import com.opnlb.website.waf.WebSiteConstants;
import com.twproject.operator.TeamworkOperator;
import com.twproject.resource.Person;
import com.twproject.resource.Resource;
import com.twproject.resource.ResourceBricks;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.ApplicationException;
import org.jblooming.anagraphicalData.AnagraphicalData;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.oql.QueryHelper;
import org.jblooming.page.ListPage;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.waf.ActionController;
import org.jblooming.waf.ActionSupport;
import org.jblooming.waf.SessionState;
import org.jblooming.waf.constants.FieldErrorConstants;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.html.display.Paginator;
import org.jblooming.waf.html.input.Uploader;
import org.jblooming.waf.view.ClientEntry;
import org.jblooming.waf.view.PageState;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.*;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 24, 2007
 * Time: 6:06:58 PM
 */
public class ResourcesImportControllerAction extends ActionSupport implements ActionController {

  public ResourcesImportControllerAction(PageState pageState) {
    super(pageState);
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)
          throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {

    Map<String, Integer> columnsPositions = new HashTable();
    List<String[]> lines = null;
    PageState pageState = PageState.getCurrentPageState(request);


    if (pageState.command == null || "READ_FILE".equals(pageState.command)) {
      pageState.sessionState.setAttribute("RESOURCE_IMPORT_LINES", null);
      pageState.sessionState.setAttribute("RESOURCE_IMPORT_CP", null);
    }

    if ("READ_FILE".equals(pageState.command)) {
        readFile(pageState, columnsPositions, pageState.sessionState);
    } else if ("IMPORT_SELECTED".equals(pageState.command)) {

      importSelected(pageState);

    }

    return pageState;
  }

  private void importSelected(PageState pageState) throws PersistenceException, org.jblooming.security.SecurityException {

    SessionState sessionState = pageState.sessionState;
    Map<String, Integer> columnsPositions = (Map<String, Integer>) sessionState.getAttribute("RESOURCE_IMPORT_CP");

    Map<String, ClientEntry> toBeCreated = pageState.getClientEntries().getEntriesStartingWithStripped("RES_CREATE_ALL_");
    Map<String, ClientEntry> toBeNamed = pageState.getClientEntries().getEntriesStartingWithStripped("RES_NAME_UPD_");
    Map<String, ClientEntry> toBeEmailed = pageState.getClientEntries().getEntriesStartingWithStripped("RES_EMAIL_UPD_");

    List<String[]> lines = (List<String[]>) sessionState.getAttribute("RESOURCE_IMPORT_LINES");
    pageState.setPage(new ListPage(lines, Paginator.getWantedPageNumber(pageState), Paginator.getWantedPageSize("RESIMPORT", pageState)));

    int lineCounterInPage = 0;
    for (Iterator iterator = pageState.getPage().getThisPageElements().iterator(); iterator.hasNext();) {

      String[] line = (String[]) iterator.next();
      //may be bugged for Out 2007 if extended formats
      String firstName = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.First_Name))];
      String lastName = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Last_Name))];
      String email = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.E__mail_Address))];

      String Business_Phone = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_Phone))];
      String Mobile_Phone = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Mobile_Phone))];

      String Business_Street = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_Street))];
      String Business_City = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_City))];
      String Business_State = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_State))];
      String Business_Postal_Code = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_Postal_Code))];
      String Business_Country = "";
      if (columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_Country))!=null)
        Business_Country = line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Business_Country))];

      String Username =  line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Username))];
      String Password =  line[columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Password))];


      if (toBeCreated.keySet().contains(lineCounterInPage + "") && toBeCreated.get(lineCounterInPage + "").checkFieldValue()) {

        createPerson(firstName, lastName, email, Business_Phone, Mobile_Phone, Business_Street,
                Business_City, Business_State, Business_Postal_Code, Business_Country,
                Username, Password,
                (TeamworkOperator)pageState.getLoggedOperator());

      } else
      if (toBeNamed.keySet().contains(lineCounterInPage + "") && toBeNamed.get(lineCounterInPage + "").checkFieldValue()) {
        Person resource = (Person) findResource(firstName, lastName, email, pageState).get(0);
        resource.setPersonName(firstName);
        resource.setPersonSurname(lastName);
        resource.store();
      } else
      if (toBeEmailed.keySet().contains(lineCounterInPage + "") && toBeEmailed.get(lineCounterInPage + "").checkFieldValue()) {
        Person resource = (Person) findResource(firstName, lastName, email, pageState).get(0);
        AnagraphicalData data = new AnagraphicalData();
        data.setIdAsNew();
        data.setOrderFactor(1);
        data.setLocationDescription("imported");
        data.setEmail(email);
        data.setTelephone(Business_Phone);
        data.setMobile(Mobile_Phone);
        data.setAddress(Business_Street);
        data.setCity(Business_City);
        data.setProvince(Business_State);
        data.setZip(Business_Postal_Code);
        data.setCountry(Business_Country);
        data.store();
        resource.getAnagraphicalDatas().add(data);
        resource.store();
      }
      lineCounterInPage++;
    }
    pageState.addMessageOK(pageState.getI18n("RESOURCE_CORRECTLY_SAVED"));
  }

  public static Person createPerson(String firstName, String lastName, String email, String business_Phone,
                                   String mobile_Phone, String business_Street, String business_City, String business_State,
                                   String business_Postal_Code, String business_Country,
                                   String username, String password,
                                   TeamworkOperator op) throws PersistenceException, org.jblooming.security.SecurityException {
    Person resource = new Person();
    resource.setIdAsNew();
    resource.setOwner(op);

    // ------------ test permissions
    resource.testPermission(op,TeamworkPermissions.resource_canCreate);
    resource.setArea(op.getDefaultAreaForPermission(TeamworkPermissions.resource_canCreate));

    resource.setPersonName(firstName);
    resource.setPersonSurname(lastName);
    resource.setNotes("imported");
    AnagraphicalData data = new AnagraphicalData();
    data.setIdAsNew();
    data.setOrderFactor(1);
    data.setLocationDescription("-");
    data.setEmail(email);
    data.setTelephone(business_Phone);
    data.setMobile(mobile_Phone);
    data.setAddress(business_Street);
    data.setCity(business_City);
    data.setProvince(business_State);
    data.setZip(business_Postal_Code);
    data.setCountry(business_Country);

    data.store();
    resource.getAnagraphicalDatas().add(data);

    if (JSP.ex(username) && JSP.ex(password) ) {
      TeamworkOperator operator = new TeamworkOperator();
      operator.setName(firstName);
      operator.setSurname(lastName);
      operator.setLoginName(username);
      operator.putOption(OperatorConstants.MEDIA_PREFERRED_CHANNEL, MessagingSystem.Media.LOG + "");
      operator.putOption(WebSiteConstants.HOME_PAGE, "personFirstStart.jsp");
      if (password != null)
        try {

          operator.changePassword(password);
          operator.store();
          resource.setMyself(operator);

        } catch (ApplicationException e) {
          e.printStackTrace();
        }


    }

    resource.store();

    return resource;
  }

  private void readFile(PageState pageState, Map<String, Integer> columnsPositions, SessionState sessionState){

    Exception exc = null;
    List<String[]> lines;
    String csvFormatTxt = pageState.getEntry("RESOURCE_IMPORT_FORMAT").stringValueNullIfEmpty();
   if(!JSP.ex(csvFormatTxt))
     csvFormatTxt = "CSV";
     try{
    FORMATS csvFormat = FORMATS.valueOf(csvFormatTxt);
      Uploader.UploadHelper instance = Uploader.getHelper("FILE_TO_IMPORT", pageState);
      if (instance == null) {
        ClientEntry ce = new ClientEntry("FILE_TO_IMPORT", null);
        ce.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
        pageState.addClientEntry(ce);
      } else {

        File temporaryFile = instance.temporaryFile;
        if (temporaryFile == null || !temporaryFile.exists()) {
          ClientEntry ce = new ClientEntry("FILE_TO_IMPORT", null);
          ce.errorCode = FieldErrorConstants.ERR_FIELD_CANT_BE_EMPTY;
          pageState.addClientEntry(ce);
        } else {

          //do import
          lines = new ArrayList();
          //read first line
          FileInputStream fis = new FileInputStream(temporaryFile);
          InputStreamReader isr = new InputStreamReader(fis, Charset.forName("UTF-8"));
          CSVParser cvsr = new CSVParser(isr);

          if (FORMATS.OUTLOOK2003.equals(csvFormat) || FORMATS.OUTLOOK2007.equals(csvFormat)) {

            String[] titles = cvsr.getLine();

            for (int i = 0; i < titles.length; i++) {
              String title = titles[i];
              columnsPositions.put(title, i);
            }
          } else if (FORMATS.THUNDERBIRD15.equals(csvFormat)) {
            ResourcesImportControllerAction.thunderColPos(columnsPositions);
          } else{
            ResourcesImportControllerAction.csvColPos(columnsPositions);
          }

          final Integer emailPos = columnsPositions.get(ResourcesImportControllerAction.realName(COLUMNS.Last_Name, csvFormat));

          //validate titles
          if (emailPos == null) {
            exc = new Exception("Error importing " + instance.originalFileName + ": need e-mail column");
          } else {
            String values[][] = cvsr.getAllValues();
            //List<String[]> l = Arrays.asList(values);
            for (int i = 0; i < values.length; i++) {
              String[] value = values[i];
              lines.add(value);
            }

            Collections.sort(lines, new Comparator<String[]>() {
              public int compare(String[] a1, String[] a2) {
                if (a1 != null && a2 != null) {
                  String email1 = a1[emailPos];
                  String email2 = a2[emailPos];
                  if (email1 != null && email2 != null)
                    return email1.compareToIgnoreCase(email2);
                }
                return 0;
              }
            });

            sessionState.setAttribute("RESOURCE_IMPORT_LINES", lines);
            sessionState.setAttribute("RESOURCE_IMPORT_CP", columnsPositions);

          }

          isr.close();
          fis.close();


        }
      }
     }
     catch (Exception e){
          pageState.addMessageError(pageState.getI18n("FILE_MALFORMED"));
     }
  }

  // fields in Thunderbird 1.5:
  // 1 Aaaron Arrosti
  // 2 O'Hara
  // 3 Aaaron Arrosti O'Hara
  // 4 Nich O'Hara
  // 5 firstMail@open-lab.com
  // 6 secondMail@open-lab.com
  // 7 055 work phone
  // 8 055 home phone
  // 9 055 fax numb
  // 10 055 pager
  // 11 333 mobile
  // 12 home street row 1
  // 13 home street row 2
  // 14 Glasgow
  // 15 SC
  // 16 50100
  // 17 United Kingdom
  // 18 Hospital street row 1
  // 19 Hospital street row 2
  // 20 GlasgowHospitalTown
  // 21 SCH
  // 22 50HOSP
  // 23 United Kingdom
  // HOSP
  // Doctor
  // Surgery department
  // Scottish Health System
  // http://www.hospital.com
  // http://www.ohara.com

  public static void thunderColPos(Map<String, Integer> columnsPositions) {

    columnsPositions.put(realName(COLUMNS.First_Name, FORMATS.OUTLOOK2003), 0);
    columnsPositions.put(realName(COLUMNS.Last_Name, FORMATS.OUTLOOK2003), 1);
    columnsPositions.put(realName(COLUMNS.E__mail_Address, FORMATS.OUTLOOK2003), 4);

    columnsPositions.put(realName(COLUMNS.Business_Phone, FORMATS.OUTLOOK2003), 6);
    columnsPositions.put(realName(COLUMNS.Mobile_Phone, FORMATS.OUTLOOK2003), 10);

    columnsPositions.put(realName(COLUMNS.Business_Street, FORMATS.OUTLOOK2003), 17);
    columnsPositions.put(realName(COLUMNS.Business_City, FORMATS.OUTLOOK2003), 19);
    columnsPositions.put(realName(COLUMNS.Business_State, FORMATS.OUTLOOK2003), 20);
    columnsPositions.put(realName(COLUMNS.Business_Postal_Code, FORMATS.OUTLOOK2003), 21);
    columnsPositions.put(realName(COLUMNS.Business_Country, FORMATS.OUTLOOK2003), 22);
  }

  public static void csvColPos(Map<String, Integer> columnsPositions) {

    columnsPositions.put(realName(COLUMNS.First_Name, FORMATS.OUTLOOK2003), 0);
    columnsPositions.put(realName(COLUMNS.Last_Name, FORMATS.OUTLOOK2003), 1);
    columnsPositions.put(realName(COLUMNS.E__mail_Address, FORMATS.OUTLOOK2003), 2);

    columnsPositions.put(realName(COLUMNS.Business_Phone, FORMATS.OUTLOOK2003), 3);
    columnsPositions.put(realName(COLUMNS.Mobile_Phone, FORMATS.OUTLOOK2003), 4);

    columnsPositions.put(realName(COLUMNS.Business_Street, FORMATS.OUTLOOK2003), 5);
    columnsPositions.put(realName(COLUMNS.Business_City, FORMATS.OUTLOOK2003), 6);
    columnsPositions.put(realName(COLUMNS.Business_State, FORMATS.OUTLOOK2003), 7);
    columnsPositions.put(realName(COLUMNS.Business_Postal_Code, FORMATS.OUTLOOK2003), 8);
    columnsPositions.put(realName(COLUMNS.Business_Country, FORMATS.OUTLOOK2003), 9);
    columnsPositions.put(realName(COLUMNS.Username, FORMATS.OUTLOOK2003), 10);
    columnsPositions.put(realName(COLUMNS.Password, FORMATS.OUTLOOK2003), 11);
  }

  // spaces became _, ' became £, / becaome $, - became __
  // Business_Country_->2007_Business_Country/Region
  // Home_Country_->2007_Home_Country/Region
  // Other_Country_->2007_Other_Country/Region

  public static enum COLUMNS {
    Title, First_Name, Middle_Name,Username, Password, Last_Name, Suffix, Company, Department, Job_Title,

    Business_Street, Business_Street_2, Business_Street_3,
    Business_City, Business_State, Business_Postal_Code, Business_Country, Home_Street, Home_Street_2, Home_Street_3, Home_City, Home_State,
    Home_Postal_Code, Home_Country, Other_Street, Other_Street_2, Other_Street_3, Other_City, Other_State, Other_Postal_Code, Other_Country,

    Assistant£s_Phone, Business_Fax, Business_Phone, Business_Phone_2, Callback, Car_Phone, Company_Main_Phone, Home_Fax, Home_Phone, Home_Phone_2,
    ISDN, Mobile_Phone, Other_Fax, Other_Phone, Pager, Primary_Phone, Radio_Phone, TTY$TDD_Phone,

    Telex, Account, Anniversary, Assistant£s_Name,
    Billing_Information, Birthday, Business_Address_PO_Box, Categories, Children, Directory_Server,

    E__mail_Address, E__mail_Type, E__mail_Display_Name,
    Email_2_Address, E__mail_2_Type, E__mail_2_Display_Name, E__mail_3_Address, E__mail_3_Type, E__mail_3_Display_Name,

    Gender, Government_ID_Number,
    Hobby, Home_Address_PO_Box, Initials, Internet_Free_Busy, Keywords, Language, Location, Manager£s_Name, Mileage, Notes, Office_Location,
    Organizational_ID_Number, Other_Address_PO_Box, Priority, Private, Profession, Referred_By, Sensitivity, Spouse, User_1, User_2, User_3, User_4, Web_Page
  }

  public static enum FORMATS {
    OUTLOOK2003, OUTLOOK2007, THUNDERBIRD15,CSV
  }

  public static String realName(COLUMNS column) {
    return realName(column, FORMATS.OUTLOOK2003);
  }

  public static String realName(COLUMNS column, FORMATS csvFormat) {

    boolean isOut2007 = "OUTLOOK2007".equals(csvFormat.toString());
    if (COLUMNS.Business_Country.equals(column) && isOut2007)
      return "Business Country/Region";
    if (COLUMNS.Home_Country.equals(column) && isOut2007)
      return "Home Country/Region";
    if (COLUMNS.Other_Country.equals(column) && isOut2007)
      return "Other Country/Region";

    String name = StringUtilities.replaceAllNoRegex(column.name(), "__", "-");
    name = StringUtilities.replaceAllNoRegex(name, "$", "/");
    name = StringUtilities.replaceAllNoRegex(name, "£", "'");
    name = StringUtilities.replaceAllNoRegex(name, "_", " ");
    return name;
  }

  public static List<Resource> findResource(String firstName, String lastName, String email, PageState pageState) throws PersistenceException {
    String hql = "select distinct resource from " + Person.class.getName() + " as resource";
    hql += " join resource.anagraphicalDatas as data";
    QueryHelper qhelp = new QueryHelper(hql);
    ResourceBricks.addSecurityClauses("resource", true, qhelp, CollectionUtilities.toList(TeamworkPermissions.resource_canRead), (TeamworkOperator) pageState.getLoggedOperator(), true, true);
    String nameSurname = "nameSurname";
    String baseFilter =
            "  (resource.personName || ' ' || resource.personSurname like :" + nameSurname +
                    " or resource.personSurname || ' ' || resource.personName like :" + nameSurname +
                    " or resource.name like :" + nameSurname +
                    " or (data.email like :email and data.email<>'')" +
                    ") and resource.hidden = :falsity ";

    qhelp.addParameter("falsity", false);
    qhelp.addOQLClause(baseFilter, nameSurname, JSP.w(firstName) + " " + JSP.w(lastName));
    qhelp.addOQLClause(baseFilter, "email", JSP.w(email));

    qhelp.addToHqlString(" order by resource.name");
    List<Resource> found = qhelp.toHql().list();
    return found;
  }

}
