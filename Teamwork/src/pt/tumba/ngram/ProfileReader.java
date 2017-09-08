package pt.tumba.ngram;

import java.io.*;
import java.util.*;

/**
 * Class to hold (static) methods to read in profile data.
 * A <code>Profile</code> stores N-gram frequency information for a given textual string.
 *
 *@author     Bruno Martins
 */
public class ProfileReader {
    
   /** The single instance of this Singleton class. */   
   private static final ProfileReader _theInstance = new ProfileReader();

   /**
    * Returns an instance of this class.
    *
    * @return An instance of <code>ProfileReader</code>.
    */
   public static ProfileReader getInstance() {
	return _theInstance;
   }

    /**
     *  Create a new N-gram from an array of bytes.
     *
     *@param  count  A <code>Map</code> with N-gram frequency information.
     *@param  ba     An array of bytes with the chars corresponding to this N-Gram.
     *@param  start  Starting position in the array of bytes.
     *@param  len    Length in the array of bytes.
     */
    protected static Map newNGram(Map count, byte[] ba, int start, int len) {
        NGram ng = NGram.newNGram(ba,start,len);
        NGram cng = (NGram)count.get(ng);
        if (cng != null) cng.inc();
        else count.put(ng,new NGram(ng));
		return count;
    }
    
    /**
     * Read an N-gram profile from an <code>InputStream</code>.
     *
     *@param  stream           The <code>InputStream</code> from where to read the Profile.
     *@return                  A <code>List</code> with the N-Grams in the profile.
     *@exception  IOException  A problem occurred while reading from the <code>InputStream</code>.
     */
    public static List read(InputStream stream) throws IOException {
        Map count = new HashMap(1000);
        BufferedInputStream bi = new BufferedInputStream(getInstance().new FilterText(stream));
        int b;
        byte ba[] = new byte[5];
        ba[4] = 42;
        int i = 0;
        while ((b = bi.read()) != -1) {
            if (b == 13 || b == 10 || b == 9) b = 32;
            i++;
            if (b != 32 || ba[3] != 32) {
                ba[0] = ba[1];
                ba[1] = ba[2];
                ba[2] = ba[3];
                ba[3] = ba[4];
                ba[4] = (byte) b;
                count = newNGram(count, ba, 4, 1);
                if (i > 1) {
                    count = newNGram(count, ba, 3, 2);
                }
                if (i > 2) {
                    count = newNGram(count, ba, 2, 3);
                }
                if (i > 3) {
                    count = newNGram(count, ba, 1, 4);
                }
                if (i > 4) {
                    count = newNGram(count, ba, 0, 5);
                }
            }
        }
        ArrayList order = new ArrayList(count.values());
        Collections.sort(order);
        return order;
    }

    /**
     * Sole constructor of ProfileReader.
     */
    private ProfileReader() {
	}
    
    public static void main ( String args[] ) throws Exception {
    	BufferedReader reader = new BufferedReader(new InputStreamReader(getInstance().new FilterText(new FileInputStream(new File("natura_publico.txt")))));
    	HashMap map = new HashMap();
    	String aux = null;
    	Vector words = new Vector();
    	int num = 0;
    	String word = "";
    	while((aux=reader.readLine())!=null) {
    		String auxs[] = aux.split("( |\\.|,|;)");
    		for(int i=0; i<auxs.length; i++ ) {
    			aux = auxs[i].trim().toLowerCase();
    			if(aux.length()<2) { word=""; num=0; continue; }
    			else if(aux.indexOf("1")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("2")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("3")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("4")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("5")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("6")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("7")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("8")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("9")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("0")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("<")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf(">")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("=")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("?")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("!")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("(")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf(")")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("[")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("]")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("{")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("}")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("\\")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("'")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("\"")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("'")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("`")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("/")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("«")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("»")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("--")!=-1) { word=""; num=0; continue; }
    			else if(aux.indexOf("]")!=-1) { word=""; num=0; continue; }
    			else {
    				if(num==0) { word = aux; num++; }
    				if(num<3) { word += " " + aux; num++; }
    				else {
    					Integer v = (Integer)(map.get(word));
    					if(v==null) v= new Integer(1); else v = new Integer(1+v.intValue());
    					map.put(word,v);
    					word = (word.substring(word.indexOf(" ")+1) + " " + aux).trim();
    				}
    			}
    		}
    	}
    	Iterator it = map.keySet().iterator();
    	while (it.hasNext()) {
    		Object o = it.next();
    		System.out.println(o+" : "+map.get(o));
    	}
    }
    
    /**
     * A Filter for Input Text, eliminating duplicate spaces and
     * subtituting '\n' and '\t' characters by spaces. 
     */
    class FilterText extends FilterInputStream {
    	
    	private int lastReadValue = -1;
    	
    	/**
    	 * Constructor for FilterText. Creates a FilterText by assigning the argument in to the field this.in so as to remember it for later use.
    	 * @param in The underlying input stream, or null if this instance is to be created without an underlying stream. 
    	 */
    	public FilterText(InputStream in) {
    		super(in);
	        lastReadValue = -1;
    	}
    	
    	/**
    	 * Tests if this input stream supports the mark and reset methods.
    	 * @return Always false.
    	 */
    	public boolean markSupported() { return false; }
    	
    	/**
    	 * Reads the next byte of data from this input stream. The value byte is returned as an int in the range 0 to 255. If no byte is available because the end of the stream has been reached, the value -1 is returned. This method blocks until input data is available, the end of the stream is detected, or an exception is thrown.
    	 * @throws IOException if an I/O error occurs.
    	 */
    	public int read() throws IOException {
    		int readValue= super.read();
    		if(readValue=='\n') readValue = ' ';
    		if(readValue=='\t') readValue = ' ';
    		if(readValue=='-' && lastReadValue=='-') readValue = '-';
    		if(readValue==' ' && lastReadValue==' ') {
    			lastReadValue = readValue;
    			return read();
    		}
    		return readValue;
    	}
    }


}
