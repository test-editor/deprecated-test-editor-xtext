package org.testeditor.rcp4

import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.Platform
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.ui.IWorkbenchPreferenceConstants
import org.eclipse.ui.PlatformUI
import org.eclipse.ui.application.IWorkbenchConfigurer
import org.eclipse.ui.application.IWorkbenchWindowConfigurer
import org.eclipse.ui.application.WorkbenchAdvisor
import org.eclipse.ui.ide.IDE
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin
import org.osgi.framework.Bundle

class ApplicationWorkbenchAdvisor extends WorkbenchAdvisor {
	static final String PERSPECTIVE_ID = "org.testeditor.rcp4.perspective"

	// $NON-NLS-1$
	override createWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
		new ApplicationWorkbenchWindowAdvisor(configurer)
	}

	override getInitialWindowPerspectiveId() {
		return PERSPECTIVE_ID
	}

	override getDefaultPageInput() {
		// makes sure the project explorer is refreshed on startup
		// otherwise the project explorer view is empty on startup and is filled only if right clicking into its view
		ResourcesPlugin.workspace.root
	}

	override initialize(IWorkbenchConfigurer configurer) {
		super.initialize(configurer)

		configurer.saveAndRestore = true
		PlatformUI.preferenceStore.setValue(IWorkbenchPreferenceConstants.SHOW_TRADITIONAL_STYLE_TABS, false)

		// here is the work around code to make the ProjectExplorer work in a non workbench perspective
		/*
		 * This is a hack to get Project tree icons to show up in the Project Explorer.
		 * It is descriped in the Eclipse Help Documents here.
		 * 
		 * http://help.eclipse.org/ganymede/topic/org.eclipse.platform.doc.isv/guide/cnf_rcp.htm
		 * 
		 */
		IDE.registerAdapters
		val ICONS_PATH = "icons/full/"
		val ideBundle = Platform.getBundle(IDEWorkbenchPlugin.IDE_WORKBENCH)
		configurer =>
			[
				declareWorkbenchImage(ideBundle, IDE.SharedImages.IMG_OBJ_PROJECT, '''«ICONS_PATH»obj16/prj_obj.gif''')
				declareWorkbenchImage(ideBundle,
					IDE.SharedImages.IMG_OBJ_PROJECT_CLOSED, '''«ICONS_PATH»obj16/cprj_obj.gif''')
				declareWorkbenchImage(ideBundle,
					IDEInternalWorkbenchImages.IMG_ETOOL_PROBLEMS_VIEW, '''«ICONS_PATH»eview16/problems_view.gif''')
				declareWorkbenchImage(ideBundle,
					IDEInternalWorkbenchImages.
						IMG_ETOOL_PROBLEMS_VIEW_ERROR, '''«ICONS_PATH»eview16/problems_view_error.gif''')
					declareWorkbenchImage(ideBundle, IDEInternalWorkbenchImages.
						IMG_ETOOL_PROBLEMS_VIEW_WARNING, '''«ICONS_PATH»eview16/problems_view_warning.gif''')
					declareWorkbenchImage(ideBundle,
						IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH, '''«ICONS_PATH»obj16/error_tsk.gif''')
					declareWorkbenchImage(ideBundle,
						IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH, '''«ICONS_PATH»obj16/warn_tsk.gif''')
				]
		}

		def declareWorkbenchImage(IWorkbenchConfigurer configurer, Bundle ideBundle, String symbolicName, String path) {
			val url = ideBundle.getEntry(path)
			val desc = ImageDescriptor.createFromURL(url)
			configurer.declareImage(symbolicName, desc, true)
		}
	}
	