package org.jblooming.waf.settings;

import org.jblooming.scheduler.Scheduler;
import org.jblooming.waf.configuration.LoaderSupport;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.AccessControlFilter;
import org.jblooming.system.ServerInfo;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.persistence.hibernate.*;
import org.hibernate.mapping.PersistentClass;
import org.hibernate.persister.entity.SingleTableEntityPersister;
import org.hibernate.type.Type;
import org.hibernate.type.DateType;
import org.hibernate.type.BooleanType;
import org.hibernate.type.StringType;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.*;
import java.sql.Statement;
import java.sql.PreparedStatement;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 11-lug-2008 : 12.19.52
 */
public class AdminControllerAction {
  public static String perform(HttpServletRequest request, HttpServletResponse response) throws Exception {

    String message = "";
    String command = request.getParameter(Commands.COMMAND);


    String psw = request.getParameter("psw");
    String filterPrefix = request.getParameter("tableExcluded");
    String focusedDb = request.getParameter("FOCUSED_DB");
    PersistenceConfiguration pcf = null;
    PersistenceContext pc = null;
    if (focusedDb != null) {
      pcf = PersistenceConfiguration.persistenceConfigurations.get(focusedDb);
      pc = new PersistenceContext(pcf.name, null);
    }

    if (psw == null || psw.trim().length() == 0)
      return "<font color=\"red\"><big>NEED PASSWORD</big></font>";

    if (!PlatformConfiguration.psw.equals(psw))
      return "";

    //in order to enable restart of session too
    if ("restart".equals(command)) {
      if (Scheduler.getInstance() != null)
        Scheduler.getInstance().stop();
      request.getSession().invalidate();
      PersistenceConfiguration.persistenceConfigurations = new LinkedHashMap();
      LoaderSupport.implementor.newInstance().start(request.getSession().getServletContext());
      message = "RESTARTED";

    } else if ("do_version_update".equals(command)) {
      try {
        HibernateFactory.getSession();
        PlatformSchemaUpdater.updateToLatestVersion();
        HibernateFactory.checkPoint();
        ApplicationState.applicationSettings.put("SETUP_DB_UPDATE_DONE", Fields.TRUE);
        ApplicationState.dumpApplicationSettings();

        message = "Update to latest version done.";
      } catch (Exception e) {
        message = "Update  was impossible:<br>" + PlatformRuntimeException.getStackTrace(e);
      }
    } else if ("server_info".equals(command)) {
      ServerInfo si = new ServerInfo();
      message = "<b>system props</b><br>" + si.systemProps(false) + "<br><br>" + "<b>system state</b><br>" + si.systemState() + "<br><br><b>total threads</b><br>" + si.listThreadDump();

      //... Add property list data to text area.
      Properties pr = System.getProperties();
      TreeSet propKeys = new TreeSet(pr.keySet());  // TreeSet sorts keys
      for (Iterator it = propKeys.iterator(); it.hasNext();) {
        String key = (String) it.next();
        message = message + key + "=" + pr.get(key) + "<br>";
      }


    } else if ("dbInfo".equals(command)) {


      if (pcf == null) {
        return "<font color=\"red\"><big>PICK DB</big></font>";
      } else {

        PlatformAnnotationConfiguration hibconfig = pcf.getHibernateConfiguration();
        message = "<big>hibernate loaded:</big><br><b>hib props:</b><br>";
        for (Object k : hibconfig.getProperties().keySet()) {
          String key = (String) k;
          if (key.startsWith("hibernate")) {
            message += "<small>" + key + ":" + hibconfig.getProperties().getProperty(key) + "</small><br>";
          }
        }
        int npr = 0;
        message += "<b>hib classes:</b><br>";
        Iterator i = hibconfig.getClassMappings();
        while (i.hasNext()) {
          PersistentClass clazz = (PersistentClass) i.next();
          message += "<small>" + clazz.getNodeName() + ":" + clazz.getTable().getName() + "&nbsp;&nbsp;</small>";
          npr++;
          if (npr == 3) {
            message += "<br>";
            npr = 0;
          }
        }
      }


    } else if ("platform_info".equals(command)) {

      for (Application app : ApplicationState.platformConfiguration.applications.values()) {

        message += "<hr><big> application:" + app.getName() + "</big ><br ><b > props:</b ><br >";
        message += "version: " + ApplicationState.getApplicationVersion() + "<br >";


        message += "<b > access, free folders:</b ><br >";
        for (String ff : AccessControlFilter.freeFolders) {
          message += ff + "<br >";
        }

        message += "<b > access, free files:</b ><br >";
        for (String ff : AccessControlFilter.freeFiles) {
          message += ff + "<br >";
        }

        message += "<b > application Parameters:</b ><br >";
        for (String ff : ApplicationState.applicationParameters.keySet()) {
          message += ff  +": &nbsp;"+ ApplicationState.applicationParameters.get(ff) + "<br >";
        }
      }


      PlatformAnnotationConfiguration hibConfiguration = (PlatformAnnotationConfiguration) HibernateFactory.getConfig();
      if (hibConfiguration!=null){
        message += "<b > JDK + Hibernate parameters:</b ><br >";
        for (String p:hibConfiguration.getProperties().stringPropertyNames()){
          message += p +": &nbsp;"+ hibConfiguration.getProperties().getProperty(p) + "<br >";
        }
      }


    } else if ("gc".equals(command)) {
      System.gc();

    } else if ("show_updates".equals(command)) {

      if (pcf == null) {
        return "<font color=\"red\"><big>PICK DB</big></font>";
      } else {
        message += HibernateUtilities.generateSchema(true, true, false, filterPrefix, request, response, pc);
      }

    } else if ("update".equals(command)) {

      message += HibernateUtilities.generateSchema(false, true, false, filterPrefix, request, response, pc);

    } else if ("export".equals(command)) {

      message += HibernateUtilities.generateSchema(true, true, true, filterPrefix, request, response, pc);

    } else if ("rebuild".equals(command)) {

      message += HibernateUtilities.generateSchema(false, true, true, filterPrefix, request, response, pc);

    } else if ("fill".equals(command)) {
      // data update

      Statement stmt = pc.session.connection().createStatement();

      Map acm = pcf.getSessionFactory().getAllClassMetadata();
      Iterator it = acm.keySet().iterator();

      while (it.hasNext()) {
        String claz = (String) it.next();

        SingleTableEntityPersister ep = (SingleTableEntityPersister) acm.get(claz);
        String table = ep.getTableName();

        if (table.startsWith(filterPrefix))
          continue;

        String propNames[] = ep.getPropertyNames();
        boolean[] nlbl = ep.getPropertyNullability();

        for (int i = 0; i < nlbl.length; i++) {
          if (!nlbl[i]) {
            String column = ep.getPropertyColumnNames(i)[0];

            String messageSQL = "";
            Type type = ep.getPropertyType(propNames[i]);

            if (type instanceof StringType) {
              messageSQL = "UPDATE " + table + " SET " + column + " = ' ' WHERE " + column + " IS NULL";
              stmt.executeUpdate(messageSQL);

            } else if (type instanceof BooleanType) {
              messageSQL = "UPDATE " + table + " SET " + column + " = 0 WHERE " + column + " IS NULL";
              stmt.executeUpdate(messageSQL);

            } else if (type instanceof DateType) {
              messageSQL = "UPDATE " + table + " SET " + column + " = ? WHERE " + column + " IS NULL";
              PreparedStatement ps = pc.session.connection().prepareStatement(messageSQL);
              ps.setDate(1, new java.sql.Date(new Date().getTime()));
              ps.execute();
            }
            message += messageSQL;
          }
        }
      }
      // t.commit();
    } else if (command.startsWith("force_release")) {

      String release = command.substring("force_release".length());
      if (JSP.ex(release)) {
        message += "<hr>Forcing update to release: " + release + "<br><br>";

        List<Release> releases = new ArrayList(PlatformSchemaUpdater.releases);
        for (Release r : PlatformSchemaUpdater.releases) {
          if (release.equals(r.releaseLabel)) {
            message += "Release found ...<br>";
            String oldValue = ApplicationState.applicationSettings.get("SETUP_DB_UPDATE_DONE");
            ApplicationState.applicationSettings.put("SETUP_DB_UPDATE_DONE", Fields.FALSE);
            PlatformSchemaUpdater.releases = new ArrayList();
            PlatformSchemaUpdater.releases.add(r);
            PlatformSchemaUpdater.updateToLatestVersion();
            ApplicationState.applicationSettings.put("SETUP_DB_UPDATE_DONE", oldValue);
            message += "Done. See messages in the logs.<br>";
          }
        }
        PlatformSchemaUpdater.releases = releases;
      }
    }

    if (pc != null)
      pc.commitAndClose();


    return message;
  }
}
