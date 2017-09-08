package org.jblooming.scheduler;

import org.jblooming.utilities.DateUtilities;
import org.jblooming.PlatformRuntimeException;

import java.io.IOException;
import java.util.Date;
import java.sql.*;

public class ExecuteSql extends ExecutableSupport {

  @Parameter("[jdbc compliant db url]")
          public String databaseUrl;

  @Parameter("")
          public String databaseUser;

  @Parameter("")
          public String databasePassword;

  @Parameter("[jdbcDriver must be available in path]")
          public String jdbcDriver;

  @Parameter("[complete sql statement]")
          public String sql;

  public JobLogData run(JobLogData jobLogData) throws IOException {

    try {
      Class.forName(jdbcDriver);
      Connection conn = DriverManager.getConnection(databaseUrl, databaseUser, databasePassword);
      Statement  s = conn.createStatement();
      s.execute(sql);
      s.close();
      conn.close();
    } catch (ClassNotFoundException e) {
      throw new PlatformRuntimeException(e);
    } catch (SQLException e) {
      throw new PlatformRuntimeException(e);
    }

    jobLogData.notes = jobLogData.notes + "Executed on " + DateUtilities.dateAndHourToString(new Date());

    return jobLogData;
  }
}
