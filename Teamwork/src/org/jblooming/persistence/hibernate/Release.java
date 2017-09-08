package org.jblooming.persistence.hibernate;

import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.mapping.Column;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.scheduler.ExecutableSupport;
import org.jblooming.scheduler.JobLogData;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PersistenceConfiguration;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jul 17, 2008
 * Time: 2:36:12 PM
 * <p/>
 * To force release launch, put "SETUP_DB_DONE_"+releaseLabel = no in global settings
 */
public class Release {

  public boolean needsToBeLaunched = false;
  public List<String> beforeHibSql = new ArrayList();
  public List<String> postHibSql = new ArrayList();
  private List<ExecutableSupport> execs = new ArrayList();
  public List<PropertyWithDefault> pwds = new ArrayList();
  public String releaseLabel;
  private boolean useSchemaBuildNumberToBeLaunched= false;

  public Release(String releaseLabel) {
    this.releaseLabel = releaseLabel;
    PlatformSchemaUpdater.releases.add(this);
  }

  /**
   * da usare nel caso non si abbia una colonna testimone per la versione
   * lancia la release se schemaBuildNumber < ApplicationState.getBuild
   */
  public void setUseSchemaBuildNumer(){
    useSchemaBuildNumberToBeLaunched=true;
  }

  public PropertyWithDefault addPropertyToCheck(Class clazz, String propertyName, Object defaultValue) {
    PropertyWithDefault pwd = new PropertyWithDefault();
    pwd.clazz = clazz;
    pwd.propertyName = propertyName;
    pwd.defaultValue = defaultValue;
    pwds.add(pwd);
    return pwd;
  }

  public PropertyWithDefault getProperty(Class clazz, String propertyName) {
    for (PropertyWithDefault propertyWithDefault : pwds) {
      if (propertyName.equalsIgnoreCase(propertyWithDefault.propertyName) && clazz.equals(propertyWithDefault.clazz))
        return propertyWithDefault;
    }
    return null;
  }


  /**
   * viene chiamato solo se non è il primo setup, ovvero non c'è ancora il db
   */
  public void verifyIfUpdateNeeded(int schemaBuildNumber) {

    Connection conn = null;
    try {
      conn = PersistenceContext.getNewConnection();
      conn.setAutoCommit(false);
      ResultSet ps = null;
      PreparedStatement pst = null;
      for (PropertyWithDefault propertyWithDefault : pwds) {
        try {

          String tableName = HibernateUtilities.getTableName(propertyWithDefault.clazz);
          propertyWithDefault.table = tableName;

          //get column name
          Column c = HibernateUtilities.getColumn(propertyWithDefault.clazz, propertyWithDefault.propertyName);
          if (c != null) {
            propertyWithDefault.column = c.getName();

            String sql = "select * from " + tableName;
            try {
              Dialect d = (Dialect) PersistenceConfiguration.getDefaultPersistenceConfiguration().dialect.newInstance();
              if (d.supportsLimit()) {
                // MySQLDialect doesn't support next method until MySQL fix Connector/J bug 
                //if (MySQLDialect.class.getName().equals(d.getClass().getName())) {
                if (d instanceof MySQLDialect) {
                  sql = "select * from " + tableName + " LIMIT 1";

                } else {
                  sql = d.getLimitString("select * from " + tableName, 0, 1);
                }

              }
            } catch (Throwable e) { // table missing -> do nothing
              Tracer.platformLogger.error(e);
              propertyWithDefault.needsToBeUpdated = false;
              needsToBeLaunched = true;
              // throw a Exception (non SQLException) in order to avoid to set field update
              throw new Exception("Table missing: '"+tableName+"'",e);
            }

            pst = conn.prepareStatement(sql);
            int count = StringUtilities.count(sql, '?');
            if (count == 1) // if there is a limit string there is also a parameter to set
              pst.setInt(1, 1);
            else if (count == 2) {
              pst.setInt(1, 0);
              pst.setInt(2, 1);
            }

            ps = pst.executeQuery();

            ResultSetMetaData metaData = ps.getMetaData();
            int cc = metaData.getColumnCount();
            boolean existsColumn = false;
            for (int i = 1; i <= cc; i++) {
              String columnName = metaData.getColumnName(i);
              if (c.getName().equalsIgnoreCase(columnName)) {
                existsColumn = true;
                break;
              }
            }
            propertyWithDefault.needsToBeUpdated = !existsColumn;
            if (!needsToBeLaunched)
              needsToBeLaunched = !existsColumn;
            ps.close();
            pst.close();
            conn.commit();
          }
        } catch (SQLException e) {
          //beautiful hack to cover cases where table itself is missing
          propertyWithDefault.needsToBeUpdated = true;
          needsToBeLaunched = true;
          throw new Exception(e);

        } catch (Throwable e) {

          if (ps != null)
            try {
              ps.close();
              pst.close();
            } catch (SQLException e1) {
              Tracer.platformLogger.warn(e1);
            }

          //may fail as schema does not exist
          //throw new PlatformRuntimeException(e);
          Tracer.platformLogger.warn("propertyWithDefault.column: " + propertyWithDefault.column, e);
        }
      }
      conn.close();

    } catch (Exception e) {

      try {
        if (conn != null && !conn.isClosed())
          conn.close();
      } catch (SQLException e1) {
        Tracer.platformLogger.warn(e1);
      }
      Tracer.platformLogger.warn(e);
    }

    //do not move -> fields to be updated MUST be tested
    if (!needsToBeLaunched)
      needsToBeLaunched = Fields.FALSE.equals(ApplicationState.applicationSettings.get("SETUP_DB_DONE_" + releaseLabel));

    //si controlla il numero di build del db se si è settato il flag "useSchemaBuildNumberToBeLaunched"
    if (!needsToBeLaunched && useSchemaBuildNumberToBeLaunched )
      needsToBeLaunched= schemaBuildNumber < Integer.parseInt(ApplicationState.getBuild());

  }

  public void schemaRefinementBeforeHibernateFactory() {
    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();
      Connection conn = pc.session.connection();

      for (String sql : beforeHibSql) {
        PreparedStatement ps = null;
        try {
          Tracer.platformLogger.debug("beforeHibSql:" + sql);
          ps = conn.prepareStatement(sql);
          ps.execute();
          ps.close();
          pc.checkPoint();
        } catch (Throwable e) {
          if (ps != null)
            try {
              ps.close();
            } catch (Throwable e1) {
              Tracer.platformLogger.warn(e);
            }
          pc.rollbackPoint();

          Tracer.platformLogger.warn(e);
        }
      }

      pc.commitAndClose();

    } catch (Exception e) {

      if (pc != null)
        pc.rollbackAndClose();
      Tracer.platformLogger.warn(e);
    }
  }


  public void propertyFillAfterHibernateFactory() {

    PersistenceContext pc = null;
    try {
      pc = new PersistenceContext();
      Connection conn = pc.session.connection();

      for (PropertyWithDefault propertyWithDefault : pwds) {

        if (propertyWithDefault.needsToBeUpdated) {
          if (propertyWithDefault.defaultValue != null) {
            PreparedStatement ps = null;
            try {
              String sql = "update " + propertyWithDefault.table + " set " + propertyWithDefault.column + " = ?";
              ps = conn.prepareStatement(sql);
              ps.setObject(1, propertyWithDefault.defaultValue);
              Tracer.platformLogger.debug("defaultValue:" + sql);
              ps.execute();
            } catch (Throwable e) {
              Tracer.platformLogger.error(e);
            } finally {
              if (ps != null)
                ps.close();
            }
          }
        }
      }
      pc.commitAndClose();
    } catch (Throwable e) {
      if (pc != null)
        pc.rollbackAndClose();
      Tracer.platformLogger.error("Update to release " + releaseLabel + " failed.");
      throw new PlatformRuntimeException(e);
    }
  }

  public void schemaRefinementAfterHibernateFactory() {

    PersistenceContext pc = null;
    try {

      for (ExecutableSupport exe : getExecs()) {

        try {
          JobLogData log = new JobLogData();
          Tracer.platformLogger.debug("before exe:" + exe.name);
          exe.run(log);
          if (log.notes != null && log.notes.length() > 0)
            Tracer.platformLogger.info(log.notes);
          Tracer.platformLogger.debug("after exe:" + exe.name);
        } catch (Throwable e) {
          Tracer.platformLogger.error(e);
        }
      }

      pc = new PersistenceContext();
      Connection conn = pc.session.connection();

      for (String sql : postHibSql) {
        PreparedStatement ps = null;
        try {
          ps = conn.prepareStatement(sql);
          Tracer.platformLogger.debug("postSql:" + sql);
          ps.execute();
          ps.close();
          pc.checkPoint();
        } catch (Throwable t) {
          if (ps != null)
            ps.close();
          if (t.getMessage() == null || !t.getMessage().toLowerCase().contains("column not found"))
            Tracer.platformLogger.error(t);
        }
      }

      pc.commitAndClose();

      if (Fields.FALSE.equals(ApplicationState.applicationSettings.get("SETUP_DB_DONE_" + releaseLabel)))
        ApplicationState.applicationSettings.put("SETUP_DB_DONE_" + releaseLabel, Fields.TRUE);

      Tracer.platformLogger.debug("Update to release " + releaseLabel + " succeeded.");

    } catch (Throwable e) {
      if (pc != null)
        pc.rollbackAndClose();
      Tracer.platformLogger.error("Update to release " + releaseLabel + " failed.");

      throw new PlatformRuntimeException(e);
    }
  }


  public List<ExecutableSupport> getExecs() {
    return execs;
  }

  public void addExec(ExecutableSupport e){
    e.name=this.releaseLabel;
    execs.add(e);
  }
}
