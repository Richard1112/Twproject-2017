package org.jblooming.utilities;

import org.apache.poi.poifs.filesystem.POIFSFileSystem;
//import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jblooming.tracer.Tracer;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.text.NumberFormat;

/**
 * Open-lab
 *
 */
public class ExcelReader {

  private Workbook workbook;


  /**
   *
   * Constructor
   *
   * @param filePath path of file to load
   */
  public ExcelReader(String filePath) {
    FileInputStream fis = null;

    try {
      fis = new FileInputStream(filePath);
      if (fis != null) {
        workbook = WorkbookFactory.create(fis);
        fis.close();
      }

    } catch (Throwable te) {
      Tracer.platformLogger.error("ExcelReader, error in constructor ", te);

    } finally {
      try {
        fis.close();
      } catch (Throwable e) {
        Tracer.platformLogger.error("ExcelReader, error in constructor when force close file", e);
      }
    }
  }


  public List<List<Object>> getExcelAsListOfRows() {

    List<List<Object>> retVal = new ArrayList();

    Sheet sheet = workbook.getSheetAt(0);

    Iterator<Row> itRow = sheet.rowIterator();

    boolean firstRow=true;
    short maxColIx=0;
    while (itRow.hasNext()) {
      Row row = itRow.next();

      List<Object> r= new ArrayList<Object>();
      maxColIx = firstRow?row.getLastCellNum():maxColIx;

      for(short colIx=0; colIx<maxColIx; colIx++) {
        Cell cell = row.getCell(colIx,Row.RETURN_BLANK_AS_NULL);
        if(cell == null) {
          r.add(null);
        } else {
          r.add(getValue(cell));
        }
      }
      retVal.add(r);
      firstRow=false;
    }

    return retVal;
  }


  /**
   * Return a string with all row of documents excel displayed in html table style
   *
   * @return String that contains table with html
   */
  public String getHtmlPreview() {
    String html = "<table border=\"1\">";

    List<List<Object>> rows = getExcelAsListOfRows();
    Sheet sheet = workbook.getSheetAt(0);

    int rowCount=0;
    for (List<Object> row:rows){

      if (rowCount == 0) {
        html = html + "<thead><td> Row</td>";
      } else {
        html = html + "<tr><td>" + rowCount + "</td>";
      }

      for (Object ob: row){
        html = html + "<td>" + ob + "</td>";
      }

      if (rowCount == 0) {
        html = html + "</thead>";
      } else {
        html = html + "</tr>";
      }

      rowCount++;
    }

    html = html + "</table>";

    return html;
  }

  /**
   *  Return string value of cell
   *
   * @param cell to check value of
   * @return String of cell's value
   */
  private Object getValue(Cell cell) {
    Object result = "";
    DataFormatter fmt = new DataFormatter();
    if (cell != null) {
      int type = cell.getCellType();

      if (type == Cell.CELL_TYPE_NUMERIC) {
        if (HSSFDateUtil.isCellDateFormatted(cell)) {
          result= cell.getDateCellValue();
        } else {
          String s = fmt.formatCellValue(cell);
          if (s.contains(",") || s.contains("."))
            result = new Double(s);
          else
            result = new Integer(s);
        }
      } else if (type == Cell.CELL_TYPE_STRING) {
        RichTextString richTextString = cell.getRichStringCellValue();
        result = richTextString.getString() + "";

      } else if (type == Cell.CELL_TYPE_BOOLEAN) {
        result = cell.getBooleanCellValue();

      } else if (type == Cell.CELL_TYPE_FORMULA) {
        result = cell.getCellFormula();

      } else if (type == Cell.CELL_TYPE_BLANK) {
        result = "blank";

      } else {
        result = "Type not supported.";
      }
    }

    return result;
  }

}
