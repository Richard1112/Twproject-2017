package com.twproject.waf.settings;


import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.base.JRBaseParameter;
import net.sf.jasperreports.engine.export.HtmlExporter;
import net.sf.jasperreports.engine.export.JRPdfExporter;
import net.sf.jasperreports.engine.util.JRLoader;
import org.jblooming.ontology.Identifiable;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.security.Permission;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.Serializable;
import java.net.URLEncoder;
import java.util.*;

public class ReportBricks {
  private static TreeSet<Report> availableReports= new TreeSet();


  public static class Report implements Comparable<Report> {
    public final String name;
    public final String type;
    public final Permission requiredPermission;
    public final File file;

    public Report(File reportFile) throws IOException {
      file=reportFile;
      name = FileUtilities.getNameWithoutExt(reportFile.getName());
      requiredPermission =new Permission("Report_"+name);
      String t="";
      String cp = reportFile.getCanonicalPath().toLowerCase();
      String[]availableTypes=new String[]{"task","resource","issue","worklog"};
      for (String ct:availableTypes){
        if (cp.contains(File.separator+ct+File.separator)){
          t=ct;
          break;
        }
      }
      this.type=t;

    }


    @Override
    public int compareTo(Report r) {
      return this.name.compareTo(r.name);  //To change body of implemented methods use File | Settings | File Templates.
    }

    public String getBestName() {
      return I18n.isActive(name)?I18n.get(name): StringUtilities.deCamel(name);
    }

    public void exportReport(String outType, Map parameters, HttpServletResponse response) throws IOException, JRException {

      parameters.put("IMG_PATH",ApplicationState.webAppFileSystemRootPath+File.separator+"img");

      JRExporter exporter = null;
      //html export
      if ("HTML".equals(outType)) {
        exporter = new HtmlExporter();
        parameters.put(JRBaseParameter.IS_IGNORE_PAGINATION, true); // in caso di html rimuove la paginazione
        response.setContentType("text/html");

        //pdf export
      } else {
        exporter = new JRPdfExporter();
        response.setContentType("application/pdf");
        response.setHeader("content-disposition", "inline; filename=" + URLEncoder.encode(name+".pdf", "UTF8"));
      }
      exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, response.getOutputStream());

      JasperReport jasperReport = (JasperReport) JRLoader.loadObjectFromFile(file.getPath());
      JasperPrint jasperPrint = JasperFillManager.fillReport(jasperReport, parameters, PersistenceContext.getDefaultPersistenceContext().session.connection());
      exporter.setParameter(JRExporterParameter.JASPER_PRINT, jasperPrint);

      exporter.exportReport();
    }


  }


  /**
   *
   * @param type Empty string for generic ones
   * @return always an array
   */
  public static TreeSet<Report> getReports(String type){
    TreeSet<Report> ret = new TreeSet<Report>();
    type= JSP.w(type); // avoid null
    for (Report r:availableReports)
      if (type.equals(r.type))
        ret.add(r);

    return ret;
  }

  public static TreeSet<Report>getAllReports(){
    return availableReports;
  }


  public static void loadAndCompileReports(){

    //svuota set reports
    availableReports.clear();

    //portal Folder
    File portalFolder = new File(ApplicationState.webAppFileSystemRootPath+"/applications/teamwork/portal/report");
    portalFolder.mkdirs();
    loadAndCompileReports(portalFolder);

    //customersFolder
    File customersFolder = new File(ApplicationState.webAppFileSystemRootPath + "/applications/teamwork/customers");
    customersFolder.mkdirs();
    loadAndCompileReports(customersFolder);

  }

  private static void loadAndCompileReports(File customersFolder) {
    // cerca report in formato sorgente
    FileFilter filter = new FileFilter() {
      public boolean accept(File pathname) {
        return pathname.isDirectory() || pathname.getName().endsWith(".jrxml");
      }
    };
    Set<File> reportFilesSources = FileUtilities.getFilesRecursively(customersFolder, filter);

    for (File report:reportFilesSources){
      // per compilare il report
      try{
        Tracer.platformLogger.debug("Compiling report "+report.getAbsolutePath());
        JasperCompileManager.compileReportToFile(report.getAbsolutePath());
      } catch (Throwable t){
        Tracer.platformLogger.error("Unable to compile report: "+report.getAbsolutePath(),t);
      }
    }

    // cerca report in formato compilato
    Set<File> reportFiles = FileUtilities.getFilesRecursively(customersFolder, new FileFilter() {
      public boolean accept(File pathname) {
        return pathname.isDirectory() || pathname.getName().endsWith(".jasper");
      }
    });

    for (File report:reportFiles){
      try {
        availableReports.add(new Report(report));
      } catch (IOException e) {
        Tracer.platformLogger.error("Error loading report "+report.getName(),e);
      }
    }
  }


  public static Report getReport(String reportName){
    Report ret=null;
    for (Report r: availableReports){
      if (reportName.equals(r.name)){
        ret=r;
        break;
      }
    }
    return ret;
  }


  public static List<Serializable>extractIds(Collection<? extends Identifiable>identifiables){
    List<Serializable> ret=new ArrayList<Serializable>();
    for (Identifiable id: identifiables)
      ret.add(id.getId());

    return ret;
  }

}
