package org.jblooming.persistence.hibernate;

import org.hibernate.HibernateException;
import org.hibernate.MappingException;
import org.hibernate.internal.util.collections.ArrayHelper;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.dialect.Dialect;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.engine.spi.Mapping;
import org.hibernate.id.IdentifierGenerator;
import org.hibernate.id.PersistentIdentifierGenerator;
import org.hibernate.id.factory.internal.DefaultIdentifierGeneratorFactory;
import org.hibernate.mapping.*;
import org.hibernate.tool.hbm2ddl.DatabaseMetadata;
import org.hibernate.tool.hbm2ddl.ForeignKeyMetadata;
import org.hibernate.tool.hbm2ddl.IndexMetadata;
import org.hibernate.tool.hbm2ddl.TableMetadata;
import org.jblooming.waf.settings.PersistenceConfiguration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Jan 2, 2007
 * Time: 4:31:50 PM
 */
public class PlatformAnnotationConfiguration extends Configuration {

  private PersistenceConfiguration persistenceConfiguration;
  public PlatformAnnotationConfiguration(PersistenceConfiguration persistenceConfiguration){
    this.persistenceConfiguration=persistenceConfiguration;
  }

  @Override
public String[] generateSchemaUpdateScript(Dialect dialect, DatabaseMetadata databaseMetadata) throws HibernateException {

    Mapping mapping = buildMapping();

    secondPassCompile();

    String defaultCatalog = getProperty(Environment.DEFAULT_CATALOG);
    //String defaultSchema = getProperty(Environment.DEFAULT_SCHEMA);

    ArrayList script = new ArrayList(50);

//    Iterator iter = getTableMappings();
//    while (iter.hasNext()) {
//      Table table = (Table) iter.next();
//      if (table.isPhysicalTable()) {
//
//        String schemaForTable = (persistenceConfiguration.schemaName != null ? persistenceConfiguration.schemaName : table.getSchema());
//        TableMetadata tableInfo = databaseMetadata.getTableMetadata(table.getName(), schemaForTable, table.getCatalog(), false);
//        if (tableInfo == null) {
//          script.add(table.sqlCreateString(dialect, mapping, defaultCatalog, schemaForTable));
//        } else {
//          Iterator subiter = table.sqlAlterStrings(dialect, mapping, tableInfo, defaultCatalog, schemaForTable);
//          while (subiter.hasNext()) script.add(subiter.next());
//        }
//
//        Iterator comments = table.sqlCommentStrings(dialect, defaultCatalog, schemaForTable);
//        while (comments.hasNext()) {
//          script.add(comments.next());
//        }
//
//      }
//    }
//
//    iter = getTableMappings();
//    while (iter.hasNext()) {
//      Table table = (Table) iter.next();
//      if (table.isPhysicalTable()) {
//        String schemaForTable = (persistenceConfiguration.schemaName != null ? persistenceConfiguration.schemaName : table.getSchema());
//
//        TableMetadata tableInfo = databaseMetadata.getTableMetadata(table.getName(), (persistenceConfiguration.schemaName != null ? persistenceConfiguration.schemaName : table.getSchema()), table.getCatalog(), false);
//
//        // Hack that generates indexes that are omitted when using
//        // hibernate.hbm2ddl.auto=update. See commented-out code below.
//
//        Iterator idxIter = table.getIndexIterator();
//        while (idxIter.hasNext()) {
//          Index index = (Index) idxIter.next();
//
//          // Skip if index already exists
//          if (tableInfo != null) {
//            IndexMetadata meta = tableInfo.getIndexMetadata(index.getName());
//            if (meta != null) {
//              continue;
//            }
//          }
//
//          script.add(index.sqlCreateString(dialect,
//                  mapping,
//                  defaultCatalog,
//                  schemaForTable));
//        }
//
//
//        if (dialect.hasAlterTable()) {
//          Iterator subIter = table.getForeignKeyIterator();
//          while (subIter.hasNext()) {
//            ForeignKey fk = (ForeignKey) subIter.next();
//            if (fk.isPhysicalConstraint()) {
//
//              boolean create = false;
//
//              if (tableInfo != null) {
//
//                ForeignKeyMetadata foreignKeyMetadata = tableInfo.getForeignKeyMetadata(fk.getName());
//                IndexMetadata indexMetadata = tableInfo.getIndexMetadata(fk.getName());
//
//                create = (foreignKeyMetadata == null && (
//                                //Icky workaround for MySQL bug:
//                                !(dialect instanceof MySQLDialect) ||
//                                        indexMetadata == null
//                        )
//                );
//              } else
//                create = true;
//
//              if (create) {
//                script.add(fk.sqlCreateString(dialect, mapping, defaultCatalog, schemaForTable));
//              }
//            }
//          }
//        }
//
//      }
//
//    }
//
//    iter = iterateGenerators(dialect);
//    while (iter.hasNext()) {
//      PersistentIdentifierGenerator generator = (PersistentIdentifierGenerator) iter.next();
//      Object key = generator.generatorKey();
//      if (!databaseMetadata.isSequence(key) && !databaseMetadata.isTable(key)) {
//        String[] lines = generator.sqlCreateStrings(dialect);
//        for (int i = 0; i < lines.length; i++) {
//          script.add(lines[i]);
//        }
//      }
//    }

    return ArrayHelper.toStringArray(script);
  }

  @Override
public Iterator iterateGenerators(Dialect
          dialect) throws MappingException {

    TreeMap generators = new TreeMap();
    String defaultCatalog = getProperty(Environment.DEFAULT_CATALOG);
    String defaultSchema = getProperty(Environment.DEFAULT_SCHEMA);

    Iterator iter = classes.values().iterator();
    while (iter.hasNext()) {
      PersistentClass pc = (PersistentClass) iter.next();

      if (!pc.isInherited()) {

        IdentifierGenerator ig = pc.getIdentifier().createIdentifierGenerator(new DefaultIdentifierGeneratorFactory(), dialect, defaultCatalog, defaultSchema, (RootClass) pc);

        if (ig instanceof PersistentIdentifierGenerator) {
          generators.put(((PersistentIdentifierGenerator) ig).generatorKey(), ig);
        }

      }
    }

    iter = collections.values().iterator();
    while (iter.hasNext()) {
      Collection collection = (Collection) iter.next();

      if (collection.isIdentified()) {

        IdentifierGenerator ig = ((IdentifierCollection) collection).getIdentifier().createIdentifierGenerator(new DefaultIdentifierGeneratorFactory(),dialect, defaultCatalog, defaultSchema, null);

        if (ig instanceof PersistentIdentifierGenerator) {
          generators.put(((PersistentIdentifierGenerator) ig).generatorKey(), ig);
        }

      }
    }

    return generators.values().iterator();
  }

}
