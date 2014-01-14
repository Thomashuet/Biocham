package fr.inria.contraintes.biocham.utils;

import fr.inria.contraintes.biocham.BiochamSplash;
import fr.inria.contraintes.biocham.customComponents.CustomToolTipButton;
import fr.inria.contraintes.biocham.filters.BiochamFileFilter;
import fr.inria.contraintes.biocham.filters.CSVFileFilter;
import fr.inria.contraintes.biocham.filters.FileFilterBiochamTrace;
import fr.inria.contraintes.biocham.filters.FileFilterDatasets;
import fr.inria.contraintes.biocham.filters.DOTFileFilter;
import fr.inria.contraintes.biocham.filters.FileFilterExcel;
import fr.inria.contraintes.biocham.filters.FileFilterExportBiocham;
import fr.inria.contraintes.biocham.filters.FileFilterImage;
import fr.inria.contraintes.biocham.filters.FileFilterImportBiocham;
import fr.inria.contraintes.biocham.filters.FileFilterPDF;
import fr.inria.contraintes.biocham.filters.ODEFileFilter;
import fr.inria.contraintes.biocham.filters.PlotFileFilter;
import fr.inria.contraintes.biocham.filters.TEXTFileFilter;
import fr.inria.contraintes.biocham.filters.XMLFileFilter;
import net.java.balloontip.styles.ModernBalloonStyle;

import java.awt.Color;
import java.awt.FileDialog;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.PixelGrabber;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;




/**
 * 
 * A class that contains utility methods and attributes that can be used by all other classes if needed.    
 * 
 * @author Dragana Jovanovska 
 * 
 */
public final class Utils {
	
	
	public static final Color foregroundColor = new Color(0,0,128).darker();
	public static final Color backgroundLighter=new Color(202,225,255);//198,226,255);
	public static final Color selectedTabColor=new Color(198,226,255);
	public static final Color tooltioGreen=new Color(102,205,170);
	public static final Color backgroundColor = new Color(176,204,247);
	public static Color inputValidatorColor=new Color(243, 255, 159);
	public static final Color menuSelectionBackground = new Color(100,149,237);
	public static final Color moleculeOperatorColor = new Color(0,139,139);
	public static final Color parameterOperatorColor = new Color(131,111,255);
	public static final Color modifiedParameterColor = new Color(0,245,255);//new Color(175,238,238);
	public static final Color refreshedColor = new Color(0,102,51);
	public static final Font rulesFont=new Font("",Font.PLAIN,14);
	public static final Font welcomeFont=new Font("",Font.BOLD,16);
	public static final Font treeExplorerFont=new Font("",Font.BOLD,14);
	public static final Font menuBarFont = new Font("",Font.BOLD,12);
	public static JFileChooser fileChooser=new JFileChooser ();;
	public static FileDialog macFileChooser;
	public static ModernBalloonStyle modern=new ModernBalloonStyle(10,10,CustomToolTipButton.color1,CustomToolTipButton.color2,CustomToolTipButton.color3);
	public static ModernBalloonStyle molecule=new ModernBalloonStyle(10,10,new Color(112,219,147),CustomToolTipButton.color2,CustomToolTipButton.color3);
	public static ModernBalloonStyle parameter=new ModernBalloonStyle(10,10,new Color(67,205,128),CustomToolTipButton.color2,CustomToolTipButton.color3);	
	public final static String OS_NAME = System.getProperty("os.name");
	public final static String OS_VERSION = System.getProperty("os.version");
	public final static boolean is_OS_LINUX = OS_NAME.contains("Linux");	
	public final static boolean is_OS_MAC = OS_NAME.contains("Mac");
	public final static boolean is_OS_WINDOWS = OS_NAME.contains("Windows");
	public final static String JAVA_VERSION=System.getProperty("java.version");
	public static String WORKING_DIR=System.getProperty("user.dir");
	public static boolean macFilechooserInitialized=false;
	public static String BIOCHAM_DIR="";
	public static String BIOCHAM_VERSION="";
	public static String DEPLOYMENT_DIR="";
	private static File EXAMPLES_DIR;
	public static Color gradientGreen=new Color(193,255,193);
	public static Color gradientBlue=new Color(198,226,255);
	public static Color gradientGray=new Color(211,211,211);
	public static Color gradientLightTurquise=new Color(224,255,255);
	public static Color gradientPink=new Color(255,228,225);
	public static Color gradientOrange=new Color(245,204,176);
	public static Color gradientViolet=new Color(216,191,216);
	public static Color gradientSand=new Color(250,235,215);
	public static Color gradientYellow=new Color(255, 250, 205);
	public static String resourcesDIR="";
	public static LinkedHashMap<String,String> ctlOperators,ltlOperators,macroOperators,ruleOperators,kineticsOperators,conditionOperators, simpleKineticsOperators, eventsOperators;
//	public static HashMap<String,String> molecules,initConcentrations,parameters;
	private static FileFilter biochamFileFilter=new BiochamFileFilter();
	private static FileFilter csvFileFilter=new CSVFileFilter();
	private static FileFilter xmlFileFilter=new XMLFileFilter();
	private static FileFilter plotFileFilter=new PlotFileFilter();
	private static FileFilter dotFileFilter=new DOTFileFilter();
//	private static FileFilter bclFileFilter=new BCLFileFilter();
	private static FileFilter txtFileFilter=new TEXTFileFilter();
	private static FileFilter odeFileFilter=new ODEFileFilter();
	private static FileFilter imageFileFilter=new FileFilterImage();
	private static FileFilter importBiochamFileFilter=new FileFilterImportBiocham();
	private static FileFilter exportBiochamFileFilter=new FileFilterExportBiocham();
	private static FileFilter biochamTraceFileFilter=new FileFilterBiochamTrace();
	private static FileFilter excellFileFilter=new FileFilterExcel();
	private static FileFilter datasetFileFilter=new FileFilterDatasets();
	private static FileFilter pdfFileFilter=new FileFilterPDF();
	public static Color[] compartmentColors=new Color[]{gradientYellow,gradientGreen,gradientBlue,gradientGray,gradientLightTurquise,gradientPink,gradientViolet,gradientOrange,gradientViolet,gradientSand};
	public static boolean mode_debug=false;
	
	public static void debugMsg(String msg){
		if(mode_debug){
			System.out.println(msg);
		}		
	}
	public static void debugMsg(int msg){
		if(mode_debug){
			System.out.println(msg);
		}		
	}
	
	public static String showOpenDialog(JFrame parent, String suffix){		
		
	
		fileChooser.resetChoosableFileFilters();
		if(is_OS_MAC){
			return showMacDialog(false,null,parent,suffix);
		}else{
			return showOtherDialog(false,null,parent,suffix);
		}
	}
	
	public static LinkedHashMap<String,String> getCtlOperators(){
		if(ctlOperators==null){
			ctlOperators=new LinkedHashMap<String,String>();			
			ctlOperators.put(" Ai( ) ", "on all paths...");
			ctlOperators.put(" Ei( ) ", "on some path...");
			ctlOperators.put(" AG( ) ", "on all paths the formula remains always true");
			ctlOperators.put(" AF( ) ", "on all paths the formula can become true");
			ctlOperators.put(" AX( ) ", "on all paths the formula is true on the next step");
			ctlOperators.put(" A(( )U( )) ", "on all paths the second formula is finally true and the first is always true until the second becomes true");
			ctlOperators.put(" EG( ) ", "on some path the formula is always true");
			ctlOperators.put(" EF( ) ", "on some path the formula can become true");
			ctlOperators.put(" EX( ) ", "on some path the formula is true on the next step");
			ctlOperators.put(" E(( )U( )) ", "on some path the second formula is finally true and the first is always true until the second becomes true");
			ctlOperators.put(" reachable( ) ", "on some path the formula can become true");
			ctlOperators.put(" stable( ) ", "on all paths the formula remains always true");
			ctlOperators.put(" steady( ) ", "on some path the formula is always true");
			ctlOperators.put(" checkpoint( , ) ", "there is no path where the first formula is false until the second is true");
			ctlOperators.put(" loop( , ) ", "approximates the oscillation property where the two formulae are alternatively true");
			ctlOperators.put(" oscil( ) ", "approximates the oscillation property zhere a formula is alternatively true and false");
			ctlOperators.put(" ! ", "negation");
			ctlOperators.put(" & ", "conjunction");
			ctlOperators.put(" | ", "disjunction");
			ctlOperators.put(" xor ", "exclusive or");
			ctlOperators.put(" -> ", "implication");
			ctlOperators.put(" <-> ", "equivalence");			
		}
		return ctlOperators;
		
	}	
	public static LinkedHashMap<String,String> getAllConditionOperators(){
		if(conditionOperators==null){
			conditionOperators=new LinkedHashMap<String,String>();	
			conditionOperators.put(" < ", null);
			conditionOperators.put(" > ", null);
			conditionOperators.put(" = ", null);
			conditionOperators.put(" =< ", null);
			conditionOperators.put(" >= ", null);
			conditionOperators.put(" and ", null);
			//conditionOperators.put(" true ", null);			
		}
		return conditionOperators;
	}
	//simpleKineticsOperators
	public static LinkedHashMap<String,String> getSimpleKineticsOperators(){
		if(simpleKineticsOperators==null){
			simpleKineticsOperators=new LinkedHashMap<String,String>();	
			simpleKineticsOperators.put(" + ", null);
			simpleKineticsOperators.put(" * ", null);			
			simpleKineticsOperators.put(" / ", null);
			simpleKineticsOperators.put(" ^ ", null);
			simpleKineticsOperators.put(" - ", null);
			simpleKineticsOperators.put(" ( ) ", null);			
			simpleKineticsOperators.put(" [ ] ", null);
			simpleKineticsOperators.put(" min( , ) ", null);
			simpleKineticsOperators.put(" max( , ) ", null);
			simpleKineticsOperators.put(" abs() ", null);
			simpleKineticsOperators.put(" log() ", null);
			simpleKineticsOperators.put(" exp() ", null);
			simpleKineticsOperators.put(" cos() ", null);
			simpleKineticsOperators.put(" sin() ", null);
			simpleKineticsOperators.put(" frac() ", null);
			simpleKineticsOperators.put(" MA() ", null);
			simpleKineticsOperators.put(" MM() ", null);
			simpleKineticsOperators.put(" H() ", null);
			simpleKineticsOperators.put(" random ", null);				
		}
		return simpleKineticsOperators;
	}
		
	public static LinkedHashMap<String,String> getAllKineticsOperators(){
		if(kineticsOperators==null){
			kineticsOperators=new LinkedHashMap<String,String>();	
			kineticsOperators.putAll(getSimpleKineticsOperators());
			kineticsOperators.put(" ( , ) ", null);			
			kineticsOperators.put(" if then ", null);
			kineticsOperators.put(" if then else", null);
			kineticsOperators.put(" if then else ( )", null);
			kineticsOperators.putAll(getAllConditionOperators());			
		}
		return kineticsOperators;
	}
	
	public static LinkedHashMap<String,String> getAllRuleOperators(){
		if(ruleOperators==null){
			ruleOperators=new LinkedHashMap<String,String>();		
			ruleOperators.put(" for ", null);
			ruleOperators.put(" : ", null);
			ruleOperators.put(" => ", null);
			ruleOperators.put(" <=> ", null);
			ruleOperators.put(" =[ ]=> ", null);
			ruleOperators.put(" =[ => ]=> ", null);
			ruleOperators.put(" <=[ ]=> ", null);
			ruleOperators.put(" _ ", null);			
			ruleOperators.putAll(getAllKineticsOperators());
			ruleOperators.put(" where ", null);
			ruleOperators.put(" in { } ", null);
			ruleOperators.put(" not in { } ", null);
			ruleOperators.put(" not in ", null);
			ruleOperators.put(" in all ", null);
			ruleOperators.put(" all_simple ", null);
			ruleOperators.put(" in parts_of{ } ",null);;
			ruleOperators.put(" diff ", null);
			ruleOperators.put(" phos_form ", null);
			ruleOperators.put(" not phos_form ",null);
			ruleOperators.put(" more_phos_than ", null);
			ruleOperators.put(" not more_phos_than ", null);
			ruleOperators.put(" submol ", null);
			ruleOperators.put(" not submol ",null);
			ruleOperators.put(" has_simple_mol_in_common ", null);
			ruleOperators.put(" has_no_simple_mol_in_common ", null);
			ruleOperators.put(" complexation ", null);
			ruleOperators.put(" decomplexation ", null);
			ruleOperators.put(" re_complexation ", null);
			ruleOperators.put(" more_elementary_interaction_rules( ) ", null);
			
			
		}		
		return ruleOperators;
	}
	public static LinkedHashMap<String,String> getAllEventsOperators(){
		if(eventsOperators==null){
			eventsOperators=new LinkedHashMap<String,String>();		
			eventsOperators.putAll(getAllKineticsOperators());
			eventsOperators.put(" Time ",null);
		
		}		
		return eventsOperators;
	}
		
	public static LinkedHashMap<String,String> getAllMacroOperators(){
		if(macroOperators==null){
			macroOperators=new LinkedHashMap<String,String>();		
			macroOperators.putAll(getSimpleKineticsOperators());
		/*	macroOperators.put(" MA( ) ", "Mass Action law function");
			macroOperators.put(" MM( , ) ", "Michaelis-Menten function");
			macroOperators.put(" H( , , ) ", "Hill function");		*/
			macroOperators.put(" sq_wave( , , , ) ", "Generates square wave signal between 2 parameters with their duration");
		}		
		return macroOperators;
	}
	public static LinkedHashMap<String,String> getLtlOperators(){
		if(ltlOperators==null){
			ltlOperators=new LinkedHashMap<String,String>();		
			ltlOperators.put(" F( ) ", "F(f) means f is finally true");
			ltlOperators.put(" G( ) ", "G(f) means f is globally true");
			ltlOperators.put(" X( ) ", "X(f) means f is true at next step");
			ltlOperators.put(" ( )U( ) ", "(f1)U(f2) means f2 is finally true and f1 is always true until f2 becomes true");
			ltlOperators.put(" oscil( , ) ", "oscillations");
			ltlOperators.put(" oscil( , , ) ", "oscillations");
			ltlOperators.put(" period( , ) ", "periodic oscillations");
			ltlOperators.put(" phase_shift( , , ) ", "phase delay");
			ltlOperators.put(" cross( , , ) ", "repetitive crossing");
			ltlOperators.put(" curve_fit( , , ) ", "curve fitting");
			ltlOperators.put(" curve_fit_error( , , , ) ", "curve fitting");
			ltlOperators.put(" ! ", "negation");
			ltlOperators.put(" & ", "conjunction");
			ltlOperators.put(" | ", "disjunction");
			ltlOperators.put(" xor ", "exclusive or");
			ltlOperators.put(" -> ", "implication");
			ltlOperators.put(" <-> ", "equivalence");
			ltlOperators.put(" < ", null);	
			ltlOperators.put(" > ", null);
			ltlOperators.put(" =< ", null);
			ltlOperators.put(" >= ", null);
			ltlOperators.put(" d( )/dt ", null);
			ltlOperators.put(" [ ] ", null);
			ltlOperators.put(" ( ) ", null);
				
		}
		return ltlOperators;
		
	}
	
	
	private static String showOtherDialog(boolean save,String name,JFrame parent,String suffix){
		
		
		if(save && name!=null){
			fileChooser.setSelectedFile(new File(name));
		}
		if(suffix!=null && suffix!=""){
			try{				
				fileChooser.setFileFilter(Utils.getFileFilter(suffix));
			}catch(Exception e){				
				fileChooser.setFileFilter(Utils.getFileFilter(suffix));
			}
		}
		String approuveButton="";
		if(save){
			fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
			fileChooser.setDialogTitle("Save");			
			approuveButton="Save";
		}else{
			fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
			fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
			fileChooser.setDialogTitle("Open");
			approuveButton="Open";
		}
	
		int resp=fileChooser.showDialog(parent,approuveButton);
		if(resp==JFileChooser.APPROVE_OPTION){
			return fileChooser.getSelectedFile().getAbsolutePath();
		}else{
			return null;
		}
		 
	}
	
	public static FileFilter getFileFilter(String suffix){
		
		if(suffix.equals(SupportedSuffixes.BIOCHAM_SUFFIX)){
			return Utils.biochamFileFilter;
		}else if(suffix.equals(SupportedSuffixes.XML_SUFFIX)){
			return Utils.xmlFileFilter;
		}else if(suffix.equals(SupportedSuffixes.CSV_SUFFIX)){
			return Utils.csvFileFilter;
		}else if(suffix.equals(SupportedSuffixes.PLOT_SUFFIX)){
			return Utils.plotFileFilter;
		}else if(suffix.equals(SupportedSuffixes.TEXT_SUFFIX)){
			return Utils.txtFileFilter;
		}else if(suffix.equals(SupportedSuffixes.ODE_SUFFIX)){
			return Utils.odeFileFilter;			
		}else if(suffix.equals("image")){
			return Utils.imageFileFilter;
		}else if(suffix.equals("importBiocham")){
			return Utils.importBiochamFileFilter;
		}else if(suffix.equals("exportBiocham")){
			return Utils.exportBiochamFileFilter;
		}else if(suffix.equals("trace")){
			return Utils.biochamTraceFileFilter;
		}else if(suffix.equals("excel")){
			return excellFileFilter;
		}else if(suffix.equals("dataset")){
			return datasetFileFilter;
		}else if(suffix.equals("pdf")){
			return pdfFileFilter;
		}else if(suffix.equals("dot")){
			return Utils.dotFileFilter;
		}else return null;
	}
	
	private static String showMacDialog(boolean save,String name,JFrame parent, String suffix){
		if(!macFilechooserInitialized){
			macFileChooser=new FileDialog(parent);
			macFilechooserInitialized=true;
		}
		
		if(suffix!=null && suffix!=""){
			try{			
				fileChooser.setFileFilter(Utils.getFileFilter(suffix));
			}catch(Exception e){				
				fileChooser.setFileFilter(Utils.getFileFilter(suffix));
			}
		}
		
		if(save && name!=null){
			macFileChooser.setFile(new File(name).getName());
		}
		if(save){
			macFileChooser.setMode(FileDialog.SAVE);
			macFileChooser.setTitle("Save");			
		}else{
			macFileChooser.setMode(FileDialog.LOAD);
			macFileChooser.setTitle("Open");		
		}
		macFileChooser.setVisible(true);
		if(macFileChooser.getFile()!=null){
			return macFileChooser.getDirectory()+macFileChooser.getFile();
		}else{
			return null;
		}
		 
	}
	
	public static String showSaveDialog(String name,JFrame p,String suffix){		
		fileChooser.resetChoosableFileFilters();
		
		if(is_OS_MAC){
			return showMacDialog(true,name,p,suffix);
		}else{
			return showOtherDialog(true,name,p,suffix);
		}
		
	}
	
	public static void checkMemoryStatus(){
		
		Runtime runtime = Runtime.getRuntime();
		long maxMemory = runtime.maxMemory();
		long allocatedMemory = runtime.totalMemory();
		long freeMemory = runtime.freeMemory();
		long totalFreeMemory = (freeMemory + (maxMemory - allocatedMemory)) / 1024;
		Utils.debugMsg("Allocated memory: "+allocatedMemory);
		Utils.debugMsg("Free memory: "+freeMemory);
		Utils.debugMsg("Total free memory: "+totalFreeMemory);
		Utils.debugMsg("MAX JVM memory: "+maxMemory);
		if(totalFreeMemory-2048<maxMemory){
			System.gc();
		}
	}
	
	public static String getFileContent(File file){
		
			StringBuilder contents = new StringBuilder();
        
			try {
				//use buffering, reading one line at a time
				//FileReader always assumes default encoding is OK!
				BufferedReader input =  new BufferedReader(new FileReader(file));
				try {
					String line = null; //not declared within while loop
					/*
					 * readLine is a bit quirky :
					 * it returns the content of a line MINUS the newline.
					 * it returns null only for the END of the stream.
					 * it returns an empty String if two newlines appear in a row.
					 */
					while (( line = input.readLine()) != null){
						contents.append(line);
						contents.append(System.getProperty("line.separator"));
					}
				}
				finally {
					input.close();
				}
			}
			catch (IOException ex){
				ex.printStackTrace();
			}
        
			return contents.toString();
		}

	
	
	
	
	/** Returns an Image, or null if the path was invalid. */
	   public static Image createImage(String path) {
		    
			
		  URL url = BiochamSplash.class.getResource(path); 		
	      if (url != null) {
	         return  Toolkit.getDefaultToolkit().createImage(url);//.getImage(url);
	      } else {
	         System.err.println("Couldn't find file: " + path);
	         return null;
	      }
	   }

	   /** Returns an ImageIcon, or null if the path was invalid. */
	   public static ImageIcon createImageIcon(String path) {
		   
			boolean b=SwingUtilities.isEventDispatchThread();
			URL imgURL = BiochamSplash.class.getResource(path);
		//System.out.println("\n UTILS:*****imageURL: "+imgURL);
		//System.out.println("\n UTILS:*****imageURL path: "+path);
	      if (imgURL != null) {
	    	  ImageIcon ic=new ImageIcon(imgURL);
	    	  imgURL=null;
	    	  return ic;
	      } else {
	    	  imgURL=null;
	    	  return null;
	      }
	   }

		public static ImageIcon createImageIcon(String filename, double d) {
				//System.out.println("\n UTILS:*****file: "+filename);
				BufferedImage bi=Utils.getScaledImage(Utils.createBufferedImage(filename), d);
				ImageIcon ic= new ImageIcon(bi);
				bi=null;
				return ic;			
		}
		
		
	   public static BufferedImage getScaledImage(BufferedImage image, double scale)
	    {
					
	        int w = (int)(image.getWidth() * scale);
	        int h = (int)(image.getHeight() * scale);
	        BufferedImage bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
	        Graphics2D g2 = bi.createGraphics();
	        g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
	                            RenderingHints.VALUE_INTERPOLATION_BILINEAR);	 
	        AffineTransform at = AffineTransform.getScaleInstance(scale, scale);
	        g2.drawRenderedImage(image, at);
	        g2.dispose();
	        g2=null;
	        at=null;
	        return bi;
	    }
	   
	  
	   public static BufferedImage createBufferedImage(String fileName)
	    {
		   
	        try
	        {	        	
	            URL url =BiochamSplash.class.getResource(fileName);
			//System.out.println("url="+url+" from filename:"+fileName);
	            return ImageIO.read(url);
	        }
	        catch(MalformedURLException mue)
	        {
	          
	            return null;
	        }
	        catch(IOException ioe)
	        {
	           
	            return null;
	        }
	    }
		
	 	
	   public static void copyfile(File f1, File f2){
		   
		   
		   try{
		     
		      InputStream in = new FileInputStream(f1);		   
		      OutputStream out = new FileOutputStream(f2);

		      byte[] buf = new byte[1024];
		      int len;
		      while ((len = in.read(buf)) > 0){
		        out.write(buf, 0, len);
		      }
		      in.close();
		      out.close();
		      in=null;
		      out=null;
		      buf=null;
		    }
		    catch(FileNotFoundException ex){
		    	ex.printStackTrace();	     
		    }
		    catch(IOException e){
		    	e.printStackTrace();		    
		    }
		}
	   
	   public static String getDeploymentDirectory(Class clazz){
		   
		   String me = clazz.getName().replace(".", "/")+".class";
		  // System.out.println("Class name deploy dirpath="+me);
	       URL dirURL = clazz.getClassLoader().getResource(me);
	      
	      // System.out.println("dir url="+dirURL);
	       //System.out.println("dir url path="+dirURL.getPath());
	      // System.out.println("dir url protocol="+dirURL.getProtocol());
	       if (dirURL.getProtocol().equals("jar")) {
		       String jarPath = dirURL.getPath().substring(5, dirURL.getPath().indexOf("!")); //strip out only the JAR file
		       //System.out.println("jarPath="+jarPath);		    
		       int ind=jarPath.lastIndexOf("/")+1;
		       String kokt=jarPath.substring(0,ind);
		       //System.out.println("returning: ind="+ind+",kokt="+kokt);
		       String result=kokt.substring(1);
		       File fff=null;
		       if(result.contains("%20")){
		    	  // System.out.println("\n\nIt contains empty spaces %20...\n\n");
		    	   fff=new File("file:/"+result);   
		    	  // System.out.println("fff path is="+fff.getAbsolutePath());
			       String path=fff.getAbsolutePath();
			       result=path.substring(0, path.indexOf("file:"));
			      // System.out.println("result="+result);
			       path=null;
		       }	
		       kokt=null;
		       fff=null;
		       me=null;
		       dirURL=null;
		       jarPath=null;
		       return result;
	       }else{
	    	   String path=dirURL.getPath();
	    	   int ind=path.indexOf("fr/inria/contraintes/biocham");
	    	   if(ind<0){
	    		   path.indexOf("fr.inria.contraintes.biocham");
	    	   }
	    	   String f=path.substring(0,ind);
	    	  // System.out.println("returning: "+(new File(f)).getPath());
	    	   
	    	   dirURL=null;
		       me=null;
		       path=null;
		       
	    	   return (new File(f)).getAbsolutePath();
	       }	     
	       
	   }
	   /**
	    * List directory contents for a resource folder. Not recursive.
	    * This is basically a brute-force implementation.
	    * Works for regular files and also JARs.
	    * 
	    * @author Greg Briggs
	    * @param clazz Any java class that lives in the same place as the resources you want.
	    * @param path Should end with "/", but not start with one.
	    * @return Just the name of each member item, not the full paths.
	    * @throws URISyntaxException 
	    * @throws IOException 
	    */
	   public static String[] getResourceListing(Class clazz, String path) throws URISyntaxException, IOException {
	       URL dirURL = clazz.getResource(path);
	       if (dirURL != null && dirURL.getProtocol().equals("file")) {
	         return new File(dirURL.toURI()).list();
	       } 

	       
	       
	       if (dirURL == null) {
	         /* 
	          * In case of a jar file, we can't actually find a directory.
	          * Have to assume the same jar as clazz.
	          */
	         String me = clazz.getName().replace(".", "/")+".class";
	         dirURL = clazz.getClassLoader().getResource(me);
	       }
	       
	       if (dirURL.getProtocol().equals("jar")) {
	         /* A JAR path */
	         String jarPath = dirURL.getPath().substring(5, dirURL.getPath().indexOf("!")); //strip out only the JAR file
	         JarFile jar = new JarFile(jarPath);
	         Enumeration<JarEntry> entries = jar.entries(); //gives ALL entries in jar
	         Set<String> result = new HashSet<String>(); //avoid duplicates in case it is a subdirectory
	         while(entries.hasMoreElements()) {
	           String name = entries.nextElement().getName();
	           if (name.contains(path)) { //filter according to the path
	             String entry = name.substring(name.lastIndexOf(File.separator)+1);
	             result.add(entry);
	           }else{
	           }
	         }
	         return result.toArray(new String[result.size()]);
	       } 
	         
	       throw new UnsupportedOperationException("Cannot list files for URL "+dirURL);
	   }
	   
	   public static String findAbsoultePathOutsidePackage(Class c,String n){
		   
		   URL dirURL = c.getResource(n);
	       if (dirURL == null) {
	         String me = c.getName().replace(".", "/")+".class";
	         dirURL = c.getClassLoader().getResource(me);
	       }
		   
	       String entry = null;  
		         String jarPath = dirURL.getPath().substring(5, dirURL.getPath().indexOf("!")); //strip out only the JAR file
		         JarFile jar = null;
				try {
					jar = new JarFile(jarPath);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		         Enumeration<JarEntry> entries = jar.entries(); 
		         while(entries.hasMoreElements()) {
		           String name = entries.nextElement().getName();
		           if (name.contains(n)) { //filter according to the path
		             entry = name.substring(name.lastIndexOf(File.separator)+1);
		             return entry;
		           }
		         }
			return entry; 
	   }
	   
	   public static String getNameWithoutExtension(String fName) {
		
					
		   return fName.substring(0,fName.indexOf("."));
	
	   }
	   
	   public static List<Object> getKeysFromValue(Map<?, ?> hm, Object value){
		    List <Object>list = new ArrayList<Object>();
		    for(Object o:hm.keySet()){
		        if(hm.get(o).equals(value)) {
		            list.add(o);
		        }
		    }
		    return list;
		  }
	   public static Object getKeyFromValue(Map<?, ?> hm, Object value){
		    //List <Object>list = new ArrayList<Object>();
		    for(Object o:hm.keySet()){
		        if(hm.get(o).equals(value)) {
		        	hm.remove(o);
		        	return o;
		        }
		    }
		    return null;
		  }
	   
	   public static File getEXAMPLES_DIR() {
		   if(EXAMPLES_DIR==null){
			   EXAMPLES_DIR=new File(Utils.BIOCHAM_DIR+File.separator+"EXAMPLES");
		   }
		   return EXAMPLES_DIR;
	   }
	
	   public static BufferedImage toBufferedImage(Image image, int width, int height) {
	        if (image instanceof BufferedImage) {return (BufferedImage)image;}
	    
	        // This code ensures that all the pixels in the image are loaded
	        image = new ImageIcon(image).getImage();
	    
	        // Determine if the image has transparent pixels
	        boolean hasAlpha = hasAlpha(image);
	    
	        // Create a buffered image with a format that's compatible with the screen
	        BufferedImage bimage = null;
	        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
	        try {
	            // Determine the type of transparency of the new buffered image
	            int transparency = Transparency.OPAQUE;
	            if (hasAlpha == true) {transparency = Transparency.BITMASK;}
	    
	            // Create the buffered image
	            GraphicsDevice gs = ge.getDefaultScreenDevice();
	            GraphicsConfiguration gc = gs.getDefaultConfiguration();
	            bimage = gc.createCompatibleImage(width, height, transparency);
	        } 
	        catch (HeadlessException e) {} //No screen
	    
	        if (bimage == null) {
	            // Create a buffered image using the default color model
	            int type = BufferedImage.TYPE_INT_RGB;
	            if (hasAlpha == true) {type = BufferedImage.TYPE_INT_ARGB;}
	            bimage = new BufferedImage(width,height, type);
	        }
	    
	        // Copy image to buffered image
	        Graphics g = bimage.createGraphics();
	    
	        // Paint the image onto the buffered image
	        g.drawImage(image, 0, 0, null);
	        g.dispose();
	    
	        return bimage;
	    }
	   
	   public static BufferedImage toBufferedImage(Image image) {
	        if (image instanceof BufferedImage) {return (BufferedImage)image;}
	    
	        // This code ensures that all the pixels in the image are loaded
	        image = new ImageIcon(image).getImage();
	    
	        // Determine if the image has transparent pixels
	        boolean hasAlpha = hasAlpha(image);
	    
	        // Create a buffered image with a format that's compatible with the screen
	        BufferedImage bimage = null;
	        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
	        try {
	            // Determine the type of transparency of the new buffered image
	            int transparency = Transparency.OPAQUE;
	            if (hasAlpha == true) {transparency = Transparency.BITMASK;}
	    
	            // Create the buffered image
	            GraphicsDevice gs = ge.getDefaultScreenDevice();
	            GraphicsConfiguration gc = gs.getDefaultConfiguration();
	            bimage = gc.createCompatibleImage(image.getWidth(null), image.getHeight(null), transparency);
	        } 
	        catch (HeadlessException e) {} //No screen
	    
	        if (bimage == null) {
	            // Create a buffered image using the default color model
	            int type = BufferedImage.TYPE_INT_RGB;
	            if (hasAlpha == true) {type = BufferedImage.TYPE_INT_ARGB;}
	            bimage = new BufferedImage(image.getWidth(null), image.getHeight(null), type);
	        }
	    
	        // Copy image to buffered image
	        Graphics g = bimage.createGraphics();
	    
	        // Paint the image onto the buffered image
	        g.drawImage(image, 0, 0, null);
	        g.dispose();
	    
	        return bimage;
	    }

	      public static boolean hasAlpha(Image image) {
	             // If buffered image, the color model is readily available
	             if (image instanceof BufferedImage) {return ((BufferedImage)image).getColorModel().hasAlpha();}
	         
	             // Use a pixel grabber to retrieve the image's color model;
	             // grabbing a single pixel is usually sufficient
	             PixelGrabber pg = new PixelGrabber(image, 0, 0, 1, 1, false);
	             try {pg.grabPixels();} catch (InterruptedException e) {}
	         
	             // Get the image's color model
	             return pg.getColorModel().hasAlpha();
	      }
	   
	   
}
