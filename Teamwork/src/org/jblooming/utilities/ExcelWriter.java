package org.jblooming.utilities;


import org.apache.poi.hssf.usermodel.*;
import org.jblooming.ontology.Identifiable;
import org.jblooming.tracer.Tracer;

import java.io.OutputStream;
import java.io.IOException;
import java.util.Date;


public class ExcelWriter {
  private OutputStream out;
  public HSSFWorkbook wb;
  public HSSFSheet sheet;
  public HSSFCellStyle cellStyle;
  private int column;
  private int rowCount = 0;

  public ExcelWriter(OutputStream out) {
		this(out, "");
  }

  public ExcelWriter(OutputStream out, String sheetName) {
    this.out = out;
    wb = new HSSFWorkbook();
		//to enable newlines you need set a cell styles with wrap=true
		// \n is the intercepted char
		cellStyle = wb.createCellStyle();
		cellStyle.setWrapText(true);
		this.sheet = wb.createSheet(null != sheetName && !"".equals(sheetName) ? sheetName : "new sheet");
  }

  public HSSFRow printHeader(String field) {
    String[] fields = {field};
    return writeRow(fields, getHeaderStyle());
  }

  public HSSFRow printHeader(String[] fields) {
    column = fields.length;
    return writeRow(fields, getHeaderStyle());
  }


  public HSSFRow println(String[] fields) {
    HSSFCellStyle style = wb.createCellStyle();
    return writeRow(fields, style);
  }

  public HSSFRow println(Object[] fields) {
    return writeRow(fields, null);
  }


  public HSSFRow println(String field) {
    String[] fields = {field};
    return writeRow(fields, null);
  }

  public void generate() {
    try {
      HSSFSheet sheet = wb.getSheetAt(0);
      for (int i = 0; i < column; i++)
        sheet.autoSizeColumn((short) i);
      wb.write(out);
      out.close();
    } catch (IOException e) {
      Tracer.platformLogger.error(e);
    }
  }

  private HSSFRow writeRow(Object[] fields, HSSFCellStyle style) {
    HSSFRow row = sheet.createRow(rowCount);
    Integer i;

    rowCount++;
    int columCount = 0;
    for (Object field : fields) {
      HSSFCell cell = row.createCell((short) columCount);
      if (style != null) {
        cell.setCellStyle(style);
        style.setDataFormat(HSSFDataFormat.getBuiltinFormat("d/m/yy"));
      } else {
        cellStyle.setVerticalAlignment(HSSFCellStyle.VERTICAL_TOP);
        cell.setCellStyle(cellStyle);
      }
      if (field instanceof Integer)
        cell.setCellValue(((Integer) field).intValue());
      else if (field instanceof Long)
        cell.setCellValue(((Long) field).longValue());
      else if (field instanceof Date) {
        cell.setCellValue((Date) field);
        if (style == null) {
          HSSFCellStyle tmp_style = wb.createCellStyle();
          tmp_style.setVerticalAlignment(HSSFCellStyle.VERTICAL_TOP);
          tmp_style.setDataFormat(HSSFDataFormat.getBuiltinFormat("m/d/yy"));
          HSSFDataFormat.getBuiltinFormats();
          cell.setCellStyle(tmp_style);
        }
      } else if (field instanceof Double)
        cell.setCellValue(((Double) field).doubleValue());
      else if (field instanceof Float)
        cell.setCellValue(((Float) field).doubleValue());
      else if (field instanceof Boolean)
        cell.setCellValue(((Boolean) field).booleanValue());
      else if (field instanceof Identifiable)
        cell.setCellValue(((Identifiable) field).getName());
      else
        cell.setCellValue(field+"");
      columCount++;
    }
    return row;
  }

  private HSSFRow writeRow(String[] fields, HSSFCellStyle style) {
    HSSFRow row = sheet.createRow(rowCount);

    rowCount++;
    int columCount = 0;
    for (String field : fields) {
      HSSFCell cell = row.createCell((short) columCount);
      if (style != null)
        cell.setCellStyle(style);
      cell.setCellValue(field);
      //cell.setCellNum();
      columCount++;
    }
    return row;
  }

  private HSSFCellStyle getHeaderStyle() {
    HSSFFont font = wb.createFont();
    font.setBoldweight(HSSFFont.BOLDWEIGHT_BOLD);
    HSSFCellStyle style = wb.createCellStyle();
    style.setFont(font);
    //style.setWrapText(true);
    return style;
  }
}
