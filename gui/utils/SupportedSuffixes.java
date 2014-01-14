package fr.inria.contraintes.biocham.utils;

import javax.imageio.ImageIO;

public final class SupportedSuffixes {

	public static String BIOCHAM_SUFFIX="bc"; 
	public static String GraphNotation_SUFFIX="bcl";
	public static String XML_SUFFIX="xml";
	public static String CSV_SUFFIX="csv";
	public static String PLOT_SUFFIX="plot";
	public static String TEXT_SUFFIX="txt";
	public static String ODE_SUFFIX="ode";
	public static String[] IMPORT_BIOCHAM={"bc","xml","ode"};
	public static String[] EXPORT_BIOCHAM={"bc","xml","ode","dot","nusmv","lotos","pl"};	
	public static String[] IMAGES=ImageIO.getWriterFormatNames();
	public static String[] TRACE_SUFFIXES={"csv","plot","xls"};
	public static String[] DATASET_SUFFIXES={"csv","txt"};
}
