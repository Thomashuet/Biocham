package fr.inria.contraintes.biocham.plotting;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.utils.Utils;
import freemarker.cache.URLTemplateLoader;
import freemarker.template.Configuration;
import freemarker.template.DefaultObjectWrapper;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.SwingUtilities;



public class PltFile {
	
	protected URLTemplateLoader getTemplateLoader() {
	    return new URLTemplateLoader() {
	      protected URL getURL(String name) {
	    	  URL url = getClass().getResource(name);
	  		Utils.debugMsg("name="+name);
	  		Utils.debugMsg("class="+getClass());
	  		Utils.debugMsg("***->url="+url);
	  	        return url;
	      }
	    };
	  }
	
	public String create(ArrayList<DataSet> dss,BiochamModel model) throws IOException, TemplateException{
				
		boolean b=SwingUtilities.isEventDispatchThread();
		
		Configuration cfg=new Configuration();
		
		cfg.setTemplateLoader(getTemplateLoader());
		cfg.setObjectWrapper(new DefaultObjectWrapper());		
		Template template=cfg.getTemplate("pltTemplate.flt");
		
		DataSet ds=null;
		Map root =new HashMap();
		List dataSets=new ArrayList();
		List coords=null;
		root.put("dataSets", dataSets);		
		for(int i=0;i<dss.size();i++){
			ds=dss.get(i);
			
			coords=new ArrayList();
			int size1=ds.getXTicks().size();
			int size2=ds.getYValues().size();
			for(int j=0;j<size1;j++){
				String k1=ds.getXTicks().get(j);
				String k2=ds.getYValues().get(j);
				String xy=k1+" "+k2;
				coords.add(new DatasetPair(xy));				
			}
			dataSets.add(new DatasetTitles(ds.getLabelName(),coords));
			
			
		}			
		File ftemp=File.createTempFile(File.separator+model.getModelName()+model.getIndex()+"GeneratedPltData",".plt");
		Utils.debugMsg("*********>>>>>>>>>>>>Plotting ile generated: "+ftemp.getAbsolutePath());
		((SimulationView)BiochamDynamicTree.currentModel.getWhoPopupSimulation()).setProgressBarDone();
		FileOutputStream generatedFile=new FileOutputStream(ftemp);
		Writer out=new OutputStreamWriter(generatedFile);
		template.process(root,out);
		ftemp.deleteOnExit();
		
		
		return ftemp.getAbsolutePath();
	}
	
	
	
	
}
