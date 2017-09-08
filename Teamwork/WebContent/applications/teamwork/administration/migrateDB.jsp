<%@ page import="org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.waf.view.PageState, java.sql.*, org.jblooming.utilities.StringUtilities, java.util.ArrayList, java.util.List" %>
<%
  //a scanso di equivoci
  if (true)return;

  PageState pageState = PageState.getCurrentPageState(request);
  pageState.getLoggedOperator().testIsAdministrator();


  //DESTINATION DB CONNECTION DATA. MUST BE a DB created by Twproject but empty.
  // W A R N I N G:  ALL DATA WILL BE REPLACED !!!!!!!!!!!!!!!
  Class.forName("com.mysql.jdbc.Driver");
  String server = "localhost";
  String db = "tw_migrate";
  String usr = "root";
  String pwd = "";



  PreparedStatement sourcePS = null;
  PreparedStatement destPS = null;

  //SOURCE the DB where Twproject is running
  Connection sourceConn = null;

  //DESTINATION a DB created by Twproject but empty. ALL DATA WILL BE REPLACED
  Connection destConn = null;

  PersistenceContext pc = PersistenceContext.getDefaultPersistenceContext();

  try {
    sourceConn = pc.session.connection();
    destConn = DriverManager.getConnection("jdbc:mysql://" + server + "/" + db + "?useUnicode=true&characterEncoding=UTF-8", usr, pwd);

    //si ferma l'integrità referenziale sulla destinazione
    destPS = destConn.prepareStatement("SET FOREIGN_KEY_CHECKS=0");
    destPS.executeUpdate();
    destPS.close();

    List<String> tables = new ArrayList();

    //si recuperano le tabelle dalla connessione
    DatabaseMetaData sourceDBMeta = sourceConn.getMetaData();
    ResultSet tablesRs = sourceDBMeta.getTables(null, null, "%", null);
    while (tablesRs.next()) {
      String tableName = tablesRs.getString(3);
      if (tableName.toLowerCase().startsWith("olpl_") ||
        tableName.toLowerCase().startsWith("twk_") ||
        tableName.toLowerCase().startsWith("flow_")) {
        tables.add(tableName);
      }
    }

    //tables = new ArrayList();
    //tables.add("twk_task");
    //tables.add("olpl_listener");

    int tableCount = 1;
    for (String table : tables) {

      //if ("olpl_listener".equalsIgnoreCase(table))
      //  continue;

      out.print(tableCount++ + ")&nbsp;&nbsp; " + table + "<br>");
      System.out.println(tableCount + ") " + table);
      try {

        //si selezionano tutte le righe dalla tabella di destinazione
        sourcePS = sourceConn.prepareStatement("select * from " + table);
        ResultSet sourceRS = sourcePS.executeQuery();

        //si svuota la tabella di destinazione
        destPS = destConn.prepareStatement("delete from " + table + "");
        destPS.executeUpdate();
        destPS.close();

        //si leggono le colonne del result set di destinazione
        PreparedStatement metaDataPs = destConn.prepareStatement("select * from " + table);
        ResultSetMetaData metaData = metaDataPs.executeQuery().getMetaData();
        int columnCount = metaData.getColumnCount();
        List<String> columns = new ArrayList();
        String columnNames = "";
        for (int col = 1; col <= columnCount; col++)
          columns.add(metaData.getColumnName(col));

        //si prepara la query di inserimento
        columnNames = StringUtilities.unSplit(columns, ",");
        String paramsValue = StringUtilities.getRepeated("?,", columnCount);

        paramsValue = paramsValue.substring(0, paramsValue.length() - 1);
        String insert = "insert into " + table + "(" + columnNames + ") values (" + paramsValue + ")";
        destPS = destConn.prepareStatement(insert);

        out.print("&nbsp;&nbsp;&nbsp;&nbsp; " + insert + "<br>");
        System.out.println("    " + insert);


        //per tutti i record sorgenti
        int count = 0;
        while (sourceRS.next()) {

          for (int i = 1; i <= columnCount; i++) {
            destPS.setObject(i, sourceRS.getObject(columns.get(i - 1)));
          }

          destPS.executeUpdate();
          count++;
          //System.out.print(".");
          if (count % 200 == 0)
            System.out.println("          "+table+ " " + count);

        }
        out.print("&nbsp;&nbsp;&nbsp;&nbsp; record importati: <b>" + count + "</b><hr>");
        System.out.println("\n          record importati: " + count);
        System.out.println("---------------------------------------------");
      } catch (Throwable t) {
        out.print("&nbsp;&nbsp;&nbsp;&nbsp; ERROR importing " + table + "<br>");
        System.out.print("ERROR importing " + table + "<br>");
        t.printStackTrace();
      } finally {
        if (destPS != null)
          destPS.close();
        if (sourcePS != null)
          sourcePS.close();

      }

    }


    //si rimette l'integrità referenziale
    destPS = destConn.prepareStatement("SET FOREIGN_KEY_CHECKS=1");
    destPS.executeUpdate();
    destPS.close();

    out.print("<hr> Import completato <hr>");
    System.out.println("\n----------------------------- IMPORT COMPLETED ----------------");


  } finally {
    if (sourcePS != null) {
      try {
        sourcePS.close();
      } catch (Throwable t) {
      }
    }
    if (destPS != null) {
      try {
        destPS.close();
      } catch (Throwable t) {
      }
    }

    /*if (sourceConn != null) {
      try {
        sourceConn.close();
      } catch (Throwable t) {
      }
    }*/
    if (destConn != null) {
      try {
        destConn.close();
      } catch (Throwable t) {
      }
    }

  }


%>
