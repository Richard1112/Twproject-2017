package com.opnlb.fulltext;

import com.opnlb.fulltext.waf.IndexingBricks;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.index.Term;
import org.apache.lucene.store.Directory;
import org.apache.lucene.util.Version;
import org.hibernate.search.FullTextSession;
import org.hibernate.search.Search;
import org.hibernate.search.SearchFactory;
import org.hibernate.search.engine.ProjectionConstants;
import org.hibernate.search.engine.spi.EntityIndexBinding;
import org.hibernate.search.indexes.spi.DirectoryBasedIndexManager;
import org.hibernate.search.indexes.spi.IndexManager;
import org.hibernate.search.spi.SearchIntegrator;
import org.jblooming.ontology.Identifiable;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;

import java.io.IOException;
import java.io.Serializable;

/**
 * Written by
 * Roberto Bicchierai rbicchierai@open-lab.com
 * Pietro Polsinelli ppolsinelli@open-lab.com
 * for the Twproject Project Management application - http://twproject.com
 */ 
public class DataForLucene {

  public Serializable id;
  public Class<? extends Identifiable> clazz;
  public Serializable areaid;
  public PersistentFile pf;

  public void indexMe() {

    PersistenceContext pc = null;
    IndexWriter writer = null;
    try {

      pc = new PersistenceContext();

      FullTextSession fullTextSession = Search.getFullTextSession(pc.session);

      clazz = (Class<? extends Identifiable>) Class.forName(ReflectionUtilities.deProxy(clazz.getName()));


      /*
      SearchFactoryIntegrator searchFactoryIntegrator = (SearchFactoryIntegrator) fullTextSession.getSearchFactory();
      EntityIndexBinding snowIndexBinder  = searchFactoryIntegrator.getIndexBinding(clazz);
      IndexManager[] indexManagers = snowIndexBinder.getIndexManagers(); // it's an array as sharding might be enabled
      DirectoryBasedIndexManager indexManager = (DirectoryBasedIndexManager) indexManagers[0];
      Directory directory = indexManager.getDirectoryProvider().getDirectory();
      */

      //GIU' ABBESTIA!!!!
      /*
      SearchFactory searchFactory= fullTextSession.getSearchFactory();
      IndexedTypeDescriptor indexedTypeDescriptor = searchFactory.getIndexedTypeDescriptor(clazz);
      String indexRootPath = PersistenceConfiguration.getFirstPersistenceConfiguration().getHibernateConfiguration().getProperties().get("hibernate.search.default.indexBase") + "";
      Directory directory = FSDirectory.open(new File(indexRootPath+File.separator+indexedTypeDescriptor.getIndexDescriptors().iterator().next().getName()));
      */

      //UN PO' MENO ABBESTIA!!
      SearchFactory searchFactory= fullTextSession.getSearchFactory();
      SearchIntegrator searchIntegrator = searchFactory.unwrap(SearchIntegrator.class);
      EntityIndexBinding indexBinder  = searchIntegrator.getIndexBinding(clazz);
      IndexManager[] indexManagers = indexBinder.getIndexManagers(); // it's an array as sharding might be enabled
      DirectoryBasedIndexManager indexManager = (DirectoryBasedIndexManager) indexManagers[0];
      Directory directory = indexManager.getDirectoryProvider().getDirectory();


      String content = TextExtractor.getContent(pf, pc);
      String guessedLanguage = IndexingBricks.guess(content);

      IndexWriterConfig indexWriterConfig = new IndexWriterConfig(Version.LUCENE_30, new SnowballHackedAnalyzer());
      indexWriterConfig.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);



      writer = new IndexWriter(directory,indexWriterConfig);

      //w = new IndexWriter(directory, new SnowballHackedAnalyzer(),false, IndexWriter.MaxFieldLength.LIMITED);

      String abstractOfContent = JSP.limWr(content, 5000);
      writer.deleteDocuments(new Term("id",id.toString()));
      writer.commit(); 
      Document doc = new Document();

      Field classField = new Field(ProjectionConstants.OBJECT_CLASS, clazz.getName(), Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(classField);

      Field docidField = new Field("id", id.toString(), Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(docidField);

      Field contentField = new Field("content", content, Field.Store.NO, Field.Index.ANALYZED);
      doc.add(contentField);

      Field absField = new Field("abstract", abstractOfContent, Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(absField);

      Field fullcontentField = new Field("fullcontent", content, Field.Store.NO, Field.Index.ANALYZED);
      doc.add(fullcontentField);

      Field pfField = new Field("persistentFile", pf.serialize(), Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(pfField);

      Field areaId = new Field("area.id", "" + areaid, Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(areaId);

      Field language = new Field("language", guessedLanguage, Field.Store.YES, Field.Index.NOT_ANALYZED);
      doc.add(language);

      writer.addDocument(doc);

    } catch (Throwable throwable) {
    	System.out.println(throwable.getMessage());
      Tracer.platformLogger.error(throwable);
    } finally {
      if (writer != null)
        try {          
          writer.close();
        } catch (IOException e) {
          Tracer.platformLogger.error(e);
        }
      if (pc != null)
        try {
          pc.commitAndClose();
        } catch (PersistenceException e) {
          Tracer.platformLogger.error(e);
        }
    }
  }

  public boolean equals(Object o) {
    return this.compareTo(o) == 0;
  }

  public int hashCode() {
    return (clazz.getName() + id).hashCode();
  }

  public int compareTo(Object o) {
    DataForLucene forLucene = ((DataForLucene) o);
    return (clazz.getName() + id + pf.serialize()).compareToIgnoreCase(forLucene.clazz.getName() + forLucene.id + forLucene.pf.serialize());
  }

}
