package org.jblooming.persistence.hibernate;

import org.hibernate.cfg.Configuration;
import org.hibernate.dialect.Dialect;
import org.hibernate.mapping.Collection;
import org.hibernate.mapping.*;
import org.hibernate.tool.hbm2ddl.DatabaseMetadata;
import org.hibernate.type.IntegerType;
import org.hibernate.type.LongType;
import org.hibernate.type.Type;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.Identifiable;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.tracer.Tracer;
import org.jblooming.uidgen.CounterHome;
import org.jblooming.utilities.*;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.constants.Fields;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.PersistenceConfiguration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;
import java.util.List;
import java.util.Set;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class HibernateUtilities {

  /*public static String generateSchema(boolean analyze, boolean catchExceptions, boolean recreateSchema, HttpServletRequest request) {
    try {
      return generateSchema(analyze, catchExceptions, recreateSchema, null, null, false, request);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
  }*/

  public static String generateSchema(boolean analyze, boolean catchExceptions, boolean recreateSchema, String filterPrefix,
                                      HttpServletRequest request, HttpServletResponse response, PersistenceContext pc) {
    try {
      return generateSchema(analyze, catchExceptions, recreateSchema, null, filterPrefix, false, request, pc);
    } catch (Exception e) {
      throw new PlatformRuntimeException(e);
    }
  }

  public static String generateSchema(boolean analyze, boolean catchExceptions, boolean recreateSchema, Method tableNameValidator, boolean feedBackOnInvalid,
                                      HttpServletRequest request, PersistenceContext pc
  ) throws Exception {
    return generateSchema(analyze, catchExceptions, recreateSchema, tableNameValidator, null, feedBackOnInvalid, request, pc);
  }

  public static String generateSchema(boolean analyze, boolean catchExceptions, boolean recreateSchema, String filterPrefix, boolean feedBackOnInvalid,
                                      HttpServletRequest request, PersistenceContext pc) throws Exception {
    return generateSchema(analyze, catchExceptions, recreateSchema, null, filterPrefix, feedBackOnInvalid, request, pc);
  }


  public static String generateSchema(
          boolean analyze,
          boolean catchExceptions,
          boolean recreateSchema,
          Method tableNameValidator,
          String filterPrefix,
          boolean feedBackOnInvalid,
          HttpServletRequest request,
          PersistenceContext pc

  ) throws Exception {
    return generateSchema(analyze,
            catchExceptions,
            recreateSchema,
            tableNameValidator,
            filterPrefix,
            feedBackOnInvalid,
            HttpUtilities.getFileSystemRootPathForRequest(request), pc);
  }

  public static String generateSchema(boolean analyze, boolean catchExceptions, boolean recreateSchema, Method tableNameValidator, String filterPrefixes,
                                      boolean feedBackOnInvalid, String rootPath, PersistenceContext pc)
          throws Exception {

    StringBuffer hist = new StringBuffer();
    hist.append("----------------------------------------------------------------------------------------------------------------\n");
    hist.append("Schema evolution: " + new Date() + "\n\n");

    String feedback = "feedback:<br>";

    Configuration store = pc.persistenceConfiguration.getHibernateConfiguration();
    Connection connection = pc.session.connection();
    String[] createSQL;

    Dialect dialect = Dialect.getDialect(store.getProperties());
    if (recreateSchema)
      createSQL = store.generateSchemaCreationScript(dialect);
    else {
      DatabaseMetadata info = new DatabaseMetadata(connection, dialect);
      createSQL = store.generateSchemaUpdateScript(dialect, info);
    }

    Set<String> launchedUpdates = new HashSet();

    for (int i = 0; i < createSQL.length; i++) {

      String sql = createSQL[i];

      if (launchedUpdates.contains(sql.toLowerCase())) {
        feedback = feedback + "operation cancelled as already done:<br><font color='orange'><small >" + sql + "</small></font><br>";
        continue;
      }

      if (tableNameValidator != null && !((Boolean) tableNameValidator.invoke(tableNameValidator.getDeclaringClass().newInstance(), sql))) {
        if (feedBackOnInvalid)
          feedback = feedback + "operation cancelled:<br><font color='gray'><small >" + sql + "</small></font><br>";
        continue;
      }

      if (filterPrefixes != null) {
        List<String> filterPrefixesL = StringUtilities.splitToList(filterPrefixes, ",");
        boolean skip = false;
        for (String filterPrefix : filterPrefixesL) {
          if (filterPrefix != null && filterPrefix.trim().length() > 0 && (sql.toUpperCase().indexOf(" " + filterPrefix.toUpperCase()) > -1
                  || sql.toUpperCase().indexOf("." + filterPrefix.toUpperCase()) > -1)) {
            if (feedBackOnInvalid)
              feedback = feedback + "operation cancelled:<br><font color='gray'><small >" + sql + "</small></font><br>";
            skip = true;
            break;
          }
        }
        if (skip)
          continue;
      }

      Tracer.platformLogger.debug(sql);

      feedback = feedback + sql + "<br>";

      if (!analyze) {
        if (catchExceptions) {

          try {
            Statement stmt = connection.createStatement();
            stmt.executeUpdate(sql);
            stmt.close();
            pc.checkPoint();
          } catch (SQLException e) {
            feedback = feedback + "<font color='red'>" + JSP.encode(e.getMessage()) + "</font><br>";
            Tracer.platformLogger.error(e);
          }
        } else {
          //hack for indexes

          try {
            Statement stmt = connection.createStatement();
            stmt.executeUpdate(sql);
            stmt.close();
          } catch (SQLException e) {
            if (sql.toLowerCase().indexOf("create index") > -1 || sql.toLowerCase().indexOf("create unique index") > -1)
              Tracer.hibernateLogger.warn("On this database index could not be created: " + sql);
            else {
              Tracer.hibernateLogger.error(sql, e);
              throw e;
            }
          }
        }
        pc.checkPoint();
      }
      launchedUpdates.add(sql.toLowerCase());
      hist.append(sql + "\n");

    }
    if (!analyze) {
      String logDirPath = rootPath + File.separator + "WEB-INF" + File.separator + "log";
      String log = logDirPath + File.separator + "schemaHistory.log";
      File logDir = new File(logDirPath);
      logDir.mkdirs();
      File file = new File(log);
      file.createNewFile();
      FileUtilities.appendToFile(log, hist.toString());

    }
    return feedback;
  }

  public static int getColumnLength(Class persClass, String property) {

    int length = 255;
    Column column = getColumn(persClass, property);
    if (column != null)
      length = column.getLength();
    return length;
  }

  public static Column getColumn(Class persClass, String property) {

    Column column = null;
    PersistentClass pc = getClassMapping(persClass);
    Property prop = pc.getProperty(property.trim());
    column = (Column) prop.getColumnIterator().next();

    //column = pc.getTable().getColumn(new Column(property));
    return column;

  }


  public static boolean isIdInteger(Class persClass) {

    PersistentClass pc = getClassMapping(persClass);
    //if (pc.getIdentifier() instanceof SimpleValue) {
    if (pc != null && pc.getIdentifier() instanceof SimpleValue) {
      SimpleValue sv = (SimpleValue) pc.getIdentifier();
      if (sv.getType() instanceof IntegerType)
        return true;
    }
    return false;
  }

  public static PersistentClass getClassMapping(Class persClass) {
    PersistenceConfiguration pcf = PersistenceConfiguration.getInstance(persClass);
    PersistentClass pc = null;
    if (pcf != null) {
      PlatformAnnotationConfiguration hibConf = pcf.getHibernateConfiguration();
      pc = hibConf.getClassMapping(persClass.getName());
      if (pc == null) {
        //consider the entity name case
        Iterator i = hibConf.getClassMappings();

        while (i.hasNext()) {
          PersistentClass classMapping = (PersistentClass) i.next();
          if (persClass.getName().equalsIgnoreCase(classMapping.getClassName())) {
            pc = classMapping;
            break;
          }
        }
      }
    }
    return pc;

  }

  public static boolean isIdAssigned(Class persClass) {

    PersistentClass pc = getClassMapping(persClass);
    if (pc.getIdentifier() instanceof SimpleValue) {
      SimpleValue sv = (SimpleValue) pc.getIdentifier();
      return "assigned".equalsIgnoreCase(sv.getIdentifierGeneratorStrategy());
    } else
      return false;
  }

  public static String counterNameForIdAssigned(Class persClass) {

    String result = null;
    PersistentClass pc = getClassMapping(persClass);
    if (pc.getIdentifier() instanceof SimpleValue) {
      SimpleValue sv = (SimpleValue) pc.getIdentifier();
      if ("assigned".equalsIgnoreCase(sv.getIdentifierGeneratorStrategy())) {
        result = pc.getTable().getName();
      }
    }
    return result;
  }

  public static SimpleValue getIdentifierGenerator(Class persClass) {

    SimpleValue result = null;
    PersistentClass pc = getClassMapping(persClass);
    if (pc.getIdentifier() instanceof SimpleValue) {
      result = (SimpleValue) pc.getIdentifier();
    }
    return result;
  }

  public static void incrementIdIfNew(Identifiable i) throws StoreException {

    if ((PersistenceHome.NEW_EMPTY_ID.equals(i.getId())) || i.getId() == null) {

      PersistentClass pc = getClassMapping(i.getClass());

      if (pc.getIdentifier() instanceof SimpleValue) {
        SimpleValue sv = (SimpleValue) pc.getIdentifier();
        if ("assigned".equalsIgnoreCase(sv.getIdentifierGeneratorStrategy())) {
          String counterName = pc.getTable().getName();

          if (sv.getType() instanceof IntegerType)
            //case int assigned
            i.setId(CounterHome.next(counterName));
          else if (sv.getType() instanceof LongType)
            //case int assigned
            i.setId((long) CounterHome.next(counterName));
          else
            //case serializable
            i.setId(CounterHome.nextSer(counterName));

        }
      }
    }
  }

  public static Type getIdType(Identifiable i) {
    return getIdType(i.getClass());
  }

  public static Type getIdType(Class clazz) {
    return getClassMapping(clazz).getIdentifier().getType();
  }

  public static java.util.Map<String, Collection> getAllInversesOnTarget(Object target) {

    Object realDelendo = ReflectionUtilities.getUnderlyingObjectAsObject(target);
    java.util.Map<String, Collection> inverses = new HashTable<String, Collection>();
    Class realClass = realDelendo.getClass();
    String targetTable = HibernateUtilities.getClassMapping(realClass).getTable().getName();

    Iterator comMapsIt = HibernateFactory.getConfig().getCollectionMappings();

    while (comMapsIt.hasNext()) {
      org.hibernate.mapping.Collection collection = (org.hibernate.mapping.Collection) comMapsIt.next();

      if (collection.isInverse() && collection.getOwner().getTable().getName().equals(targetTable))
        inverses.put(collection.getNodeName(), collection);
    }
    return inverses;
  }


  public static String getTableName(Class persistentClass) {
    PersistentClass classMapping = HibernateUtilities.getClassMapping(persistentClass);

    if (classMapping != null)
      return classMapping.getTable().getName();
    else
      return null;
  }

  /**
   * Filters the src collection and puts the objects matching the
   * clazz into the dest collection.
   */
  public static <T> void filterHibernateCollection(Class<T> clazz,
                                                   java.util.Collection<?> src,
                                                   java.util.Collection<T> dest) {
    for (Object o : src) {
      o = ReflectionUtilities.getUnderlyingObject(o);
      if (clazz.isInstance(o)) {
        dest.add(clazz.cast(o));
      }
    }
  }

  /**
   * Filters the src collection and puts all matching objects into
   * an ArrayList, which is then returned.
   */
  public static <T> java.util.Collection<T> filterHibernateCollection(Class<T> clazz,
                                                                      java.util.Collection<?> src) {
    java.util.Collection<T> result = new ArrayList<T>();
    filterHibernateCollection(clazz, src, result);
    return result;
  }


  public static String generateDropInxed(Class classToDeindexed, String indexName) {

    String sql = null;

    try {

      // get PersitentClass
      PersistenceConfiguration configuration = PersistenceConfiguration.getDefaultPersistenceConfiguration();
      PersistentClass pClass = configuration.getHibernateConfiguration().getClassMapping(classToDeindexed.getName());

      Table table = pClass.getTable();

      // this string is the same of Index.buildSqlDropIndexString, but they won't fix this damnd problem
      //sql = "drop index " + (d.qualifyIndexName() ? StringHelper.qualify(table.getQualifiedName(d, defaultCatalog, defaultSchema), indexName) : indexName);

      String dialectClassName = configuration.dialect.getName();

      if (dialectClassName.indexOf("Oracle") >= 0) {
        sql = "drop index " + indexName;
      } else if (dialectClassName.toLowerCase().indexOf("mysql") >= 0) {
        sql = "drop index " + indexName + " on " + table.getName();
      } else {
        //String defaultCatalog = table.getCatalog() != null ? table.getCatalog() : Environment.DEFAULT_CATALOG;
        String defaultCatalog = table.getCatalog() != null ? table.getCatalog() : null;
        String defaultSchema = PersistenceConfiguration.getDefaultPersistenceConfiguration().schemaName != null ? PersistenceConfiguration.getDefaultPersistenceConfiguration().schemaName : table.getSchema();
        // I've used this method: completly bugged! :(
        sql = Index.buildSqlDropIndexString((Dialect) configuration.dialect.newInstance(), table, indexName, defaultCatalog, defaultSchema);
      }

      if (sql == null || sql.equalsIgnoreCase("")) {
        sql = "drop index " + table.getName() + "." + indexName;
      }

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
    }

    return sql;
  }



  public static String generateDropForeignKey(Class classToDeindexed, String fkName) {

    String sql = "ALTER TABLE ";

    try {

      // get PersitentClass
      PersistenceConfiguration configuration = PersistenceConfiguration.getDefaultPersistenceConfiguration();
      PersistentClass pClass = configuration.getHibernateConfiguration().getClassMapping(classToDeindexed.getName());

      Table table = pClass.getTable();

      sql=sql+ table.getName() + " DROP ";

      String dialectClassName = configuration.dialect.getName();


      if (dialectClassName.toLowerCase().indexOf("mysql") >= 0) {
        sql+= "FOREIGN KEY ";
      } else {
        sql+=" CONSTRAINT ";
      }

      sql += fkName;
      

    } catch (Throwable t) {
      Tracer.desperatelyLog("", true, t);
    }

    return sql;
  }
}
