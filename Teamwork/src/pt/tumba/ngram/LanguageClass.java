package pt.tumba.ngram;

import java.io.*;

/**
 * Understanding the language of a given document is an essential
 * step in working with unstructured multilingual text. Without this basic knowledge,
 * applications such as information retrieval and text mining will not be able to accurately
 * process the data, potentially leading to a loss of critical information.</p><p>
 * 
 * This is a simple utility to find out which language a text is written in. By incorporating it,
 * applications can take a fully automated approach to processing unknown text by quickly
 * and accurately determining the language of incoming data. </p><p>
 * 
 * Essentially, the classifier relyes on the classification technique described in 
 * Cavnar & Trenkle, "N-Gram-Based Text Categorization".  The central idea of the
 * Cavnar & Trenkle technique is to calculate a language "fingerprint" of an unknown document,
 * and compare this with the fingerprints of a number of documents of which the language is
 * known. The language of the closest matche is output as the classification. A 
 * fingerprint is a list of the most frequent n-grams occurring in a document, ordered 
 * by frequency.</p><p> 
 * 
 * For reliable language guessing (at least with sufficiently different languages) the classifier
 * only needs a few kilobytes max, so don't feed it 100KB of text. However, a problem occurs
 * while trying to separate very similar languages (i.e. Portuguese from Portugal and from Brasil).
 * In order to deal with this latter case, <code>LanguageClass</code> actually consists of
 * two separate classifiers: one for the general case, and another for discriminating Portuguese
 * from Portugal and from Brasil. The latter one also uses the technique proposed by
 * Cavnar & Trenkle, but N-Gram profiles are speficially build in order to select the most 
 * descriminative N-grams. 
 *
 * @author Bruno Martins
 */
public class LanguageClass {

	/** The classifyer for the general case. */
	private NGramCathegorizer cat1;
	
	/** The classifyer for discriminating both versions of Portuguese (Portugal and Brasil). */
	private NGramCathegorizer cat2;
	
	/** Path for the datafiles for the general case classifyer. */
	private String directory1 = "language-profiles";

	/** Path for the datafiles for the Portuguese classifyer. */
	private String directory2 = "language-profiles" + File.separator + "pt-br";

	/**
	 * Constructor for <code>LanguageClass</code>.
	 *
	 * @throws Exception A problem occured while reading the datafiles.
	 */
	public LanguageClass () throws Exception {
		cat1 = new NGramCathegorizer(directory1);
		try {
			cat2 = new NGramCathegorizer(directory2);
		} catch ( Exception e ) {
			cat2 = null;
		}
	}

	/**
	 * Constructor for <code>LanguageClass</code>.
	 *
     * @param dir Base path for the datafiles.
	 * @throws Exception A problem occured while reading the datafiles.
	 */
	public LanguageClass ( String dir ) throws Exception {
		directory1 = dir;
		cat1 = new NGramCathegorizer(directory1);
		try {
			if(dir.endsWith(File.separator))
        directory2 = dir + "pt-br";
      else
        directory2 = dir + File.separator + "pt-br";
			//cat2 = new NGramCathegorizer(directory2);
		} catch ( Exception e ) {
			cat2 = null;
		}
	}

	/**
	 * Constructor for <code>LanguageClass</code>.
	 *
	 * @param dir1 Path for the datafiles for the general case classifyer.
	 * @param dir2 Path for the datafiles for the Portuguese classifyer.
	 * @throws Exception A problem occured while reading the datafiles.
	 */
	public LanguageClass ( String dir1, String dir2 ) throws Exception {
		directory1 = dir1;
		directory2 = dir2;
		cat1 = new NGramCathegorizer(directory1);
		try {
			cat2 = new NGramCathegorizer(directory2);
		} catch ( Exception e ) {
			cat2 = null;
		}
	}
	
	/**
	 * Guess the language of a String of text.
	 *
	 * @param text A String of text.
	 * @return The language for the given String of text.
	 * @throws Exception A problem occured with the classifyer.
	 */
	public String classify ( String text ) throws Exception {
		InputStream str = new BufferedInputStream(new ByteArrayInputStream(text.getBytes()));
		Profile prof = new EntryProfile(str, NGramConstants.USEDNGRAMSMAX, NGramConstants.USEDNGRAMSMIN);
		Profile res = cat1.match(prof);
		if(res.toString().startsWith("portuguese") && cat2!=null) {
				str = new BufferedInputStream(new ByteArrayInputStream(text.getBytes()));
				Profile prof2 = new EntryProfile(str,-1,-1);
				return classify(prof,prof2);
		} 
		return res.toString();
	}
	
	/**
	 * Guess the language of a text File.
	 *
	 * @param file A text File.
	 * @return The language for the given text File.
	 * @throws Exception A problem occured with the classifyer.
	 */
	public String classify ( File file ) throws Exception {
		InputStream str = new BufferedInputStream(new FileInputStream(file));
		Profile prof = new EntryProfile(str, NGramConstants.USEDNGRAMSMAX, NGramConstants.USEDNGRAMSMIN);
		Profile res = cat1.match(prof);
		if(res.toString().startsWith("portuguese") && cat2!=null) {
				str = new BufferedInputStream(new FileInputStream(file));
				Profile prof2 = new EntryProfile(str,-1,-1);
				return classify(prof,prof2);
		}
		return res.toString();
	}
	
	/**
	 * Guess the language of a given text according to its NGram profile.
	 * 
	 * @param prof An NGram profile.
	 * @return The language of a given text according to its NGram profile.
	 * @throws Exception A problem occured with the classifyer.
	 */
	public String classify ( Profile prof ) throws Exception {
		return classify(prof,null);
	}
		
	/**
	 * Guess the language of a given text according to its NGram profile.
	 * 
	 * @param prof An NGram profile.
	 * @param prof2 A "most informative" NGram profile.
	 * @return The language of a given text according to its NGram profile.
	 * @throws Exception A problem occured with the classifyer.
	 */
	public String classify ( Profile prof, Profile prof2 ) throws Exception {
		Profile res = cat1.match(prof);
		if(cat2!=null && res.toString().startsWith("portuguese")) {
			Profile res2; 
			if(prof2==null)	res2 = cat2.match(prof);
			else res2 = cat2.match(prof2);
			if(res2!=null) return res2.toString();
		}
		return res.toString();
	}

	/**
	 * The main method of the Language Classifier.
	 * 
	 *@param args The command line arguments, tokenized.
	 *@throws Exception A problem occured with the classifier.
	 */
	public static void main ( String args[] ) throws Exception {
		try {
			int i = 0;
			LanguageClass cath;
			if(args.length>0 && args[0].startsWith("-data=")) {
				cath = new LanguageClass(args[0].substring(6));
				i++;
			}
			cath = new LanguageClass();
			for (; i < args.length; i++) {
				System.out.println("Best match for " + args[i] + " is : " + cath.classify(new File(args[i])));
			}
		} catch (Exception e) {
			System.err.println("Terminated by " + e);
			e.printStackTrace();
		}
	}

}
