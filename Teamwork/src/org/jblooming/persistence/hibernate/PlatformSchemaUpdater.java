package org.jblooming.persistence.hibernate;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.Counter;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Open Lab
 * info@open-lab.com
 * Date: Sep 7, 2006
 * Time: 3:57:17 PM
 */
public class PlatformSchemaUpdater {

  public static List<Release> releases = new ArrayList();

  public static void updateToLatestVersion() {

    int schemaBuildNumber=getSchemaBuildNumber();

    boolean firstSetup = false;
    boolean updateSchema = false;
    boolean doSetup = !Fields.TRUE.equals(ApplicationState.applicationSettings.get("SETUP_DB_UPDATE_DONE"));

    //check if olpl_operator exists
    if (!existsTable("olpl_operator"))
      firstSetup = true;


    if (!firstSetup)
      for (Release r : releases) {
        r.verifyIfUpdateNeeded(schemaBuildNumber);
        updateSchema = r.needsToBeLaunched || updateSchema;
        if (r.needsToBeLaunched || doSetup) {
          r.schemaRefinementBeforeHibernateFactory();
        }
      }

    if (updateSchema || doSetup || firstSetup) {
      PersistenceContext pc = null;
      try {
        pc = new PersistenceContext();
        HibernateUtilities.generateSchema(false, false, false, null, "tw_,testSuite_", true, ApplicationState.webAppFileSystemRootPath, pc);
        pc.commitAndClose();
      } catch (Throwable e) {
        if (pc != null)
          pc.rollbackAndClose();
        Tracer.logExceptionOnPlatformOrOther(e);
        throw new PlatformRuntimeException(e);
      }
    }

    if (!firstSetup)
      for (Release r : releases) {
        if (r.needsToBeLaunched)
          r.propertyFillAfterHibernateFactory();
      }


    if (!firstSetup)
      for (Release r : releases) {
        if (r.needsToBeLaunched)
          r.schemaRefinementAfterHibernateFactory();

      }


    // si memorizza sul db lo SCHEMA_BUILD_NUMBER
    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();
      Counter counter=(Counter) PersistenceHome.findUnique(Counter.class, "name", "SCHEMA_BUILD_NUMBER",pc);
      if (counter==null)
        counter= new Counter("SCHEMA_BUILD_NUMBER");
      counter.setValue(Integer.parseInt(ApplicationState.getBuild()));
      counter.store(pc);
      pc.commitAndClose();
    } catch (Throwable e) {
      if (pc != null)
        pc.rollbackAndClose();
      Tracer.logExceptionOnPlatformOrOther(e);
      throw new PlatformRuntimeException(e);
    }

  }


  public static boolean isSomeUpdateNeeded() {
    for (Release r : releases) {
      if (r.needsToBeLaunched)
        return true;
    }
    return false;
  }

  public static boolean existsTable(String tableName) {
    boolean ret = false;

    Connection conn = null;
    try {
      conn = PersistenceContext.getNewConnection();
      conn.setAutoCommit(false);

      DatabaseMetaData md = conn.getMetaData();
      ResultSet rs = md.getTables(null, null, "%", null);
      while (rs.next()){
        if(tableName.equalsIgnoreCase(rs.getString(3))){
          ret=true;
          break;
        }
      }
      rs.close();
    } catch (Throwable t) {
      Tracer.platformLogger.error("", t);
    } finally {
      try {
        if (conn != null){
          conn.rollback();
          conn.close();
        }
      } catch (Throwable t) {
        Tracer.platformLogger.error("", t);
      }
    }
    return ret;
  }

  /**
   *
   * @return il numero dell'ultima release con cui è stato aggiornato il db
   */
  public static int getSchemaBuildNumber() {
    int ret = 0;

    Connection conn = null;
    try {
      conn = PersistenceContext.getNewConnection();
      conn.setAutoCommit(false);

      ResultSet resultSet = conn.prepareStatement("select valuex from olpl_counter where id='SCHEMA_BUILD_NUMBER' ").executeQuery();
      if (resultSet.next())
        ret=resultSet.getInt("valuex");

      resultSet.close();
    } catch (Throwable t) {
      Tracer.platformLogger.error("", t);
    } finally {
      try {
        if (conn != null){
          conn.close();
        }
      } catch (Throwable t) {
        //Tracer.platformLogger.error("", t);
      }
    }
    return ret;
  }


}
