#!groovy

/**

    Requirements for this Jenkinsfile:
    - JDK8 with the id "jdk8"
    - Maven 3.2.5 with the id "Maven 3.2.5"
    - Plugin "Pipeline Utility Steps"
    - Plugin "Xvfb" with an installation called "System"

    The following signatures need to be approved:
    method hudson.plugins.git.GitSCM getUserRemoteConfigs
    method hudson.plugins.git.UserRemoteConfig getUrl
    method java.util.Collection addAll java.util.Collection
    staticMethod org.codehaus.groovy.runtime.DefaultGroovyMethods first java.util.List
    staticMethod org.codehaus.groovy.runtime.DefaultGroovyMethods stripIndent java.lang.String
*/

nodeWithProperWorkspace {
    stage 'Checkout'
    checkout scm
    if (isMaster()) {
        // git by default checks out detached, we need a local branch
        sh "git checkout $env.BRANCH_NAME" // workaround for https://issues.jenkins-ci.org/browse/JENKINS-31924
        sh 'git fetch --prune origin +refs/tags/*:refs/tags/*' // delete all local tags
        sh "git reset --hard origin/master"
        if (isTag()) {
            // Workaround: we don't want infinite releases.
            echo "Aborting build as the current commit on master is already tagged."
            return
        }
    }
    sh "git clean -ffdx"

    def preReleaseVersion = getCurrentVersion()
    if (isMaster()) {
        prepareRelease()
    }

    stage 'Build target platform'
    withMavenEnv {
        mvn 'clean install -f "releng/org.testeditor.releng.target/pom.xml"'
    }

    stage (isMaster() ? 'Build and deploy' : 'Build')
    withMavenEnv(["MAVEN_OPTS=-Xms512m -Xmx2g"]) {
        def goal = isMaster() ? 'deploy' : 'install'
        withXvfb {
            mvn "clean $goal -Dmaven.test.failure.ignore -Dsurefire.useFile=false -Dtycho.localArtifacts=ignore"
        }
    }
    
    // workaround for now to speed-up the build: only build the product on develop and master
    def buildProduct = env.BRANCH_NAME == 'develop' || isMaster()
    if (buildProduct) {
        stage 'Build product'
        withMavenEnv(["MAVEN_OPTS=-Xms512m -Xmx2g"]) {
            mvn 'package -Pproduct -DskipTests -Dtycho.localArtifacts=ignore'
        }
    }

    if (isMaster()) {
        postRelease(preReleaseVersion)
    }

    stage 'Archive results'
    if (buildProduct) {
        archive '**/target/products/*.zip'
    }
    step([$class: 'JUnitResultArchiver', testResults: '**/target/surefire-reports/TEST-*.xml'])
    codecov('codecov_test-editor-xtext')
}

boolean isMaster() {
    return env.BRANCH_NAME == 'master'
}

void prepareRelease() {
    stage 'Prepare release'

    // Remove SNAPSHOT version
    echo 'Removing SNAPSHOT from target platform version'
    def String noSnapshotVersion = '\\${parsedVersion.majorVersion}.\\${parsedVersion.minorVersion}.\\${parsedVersion.incrementalVersion}'
    setVersion(noSnapshotVersion, 'releng/org.testeditor.releng.target/pom.xml', 'org.testeditor.releng.target.parent')
    echo 'Removing SNAPSHOT from test-editor version'
    setVersion(noSnapshotVersion, 'pom.xml', 'org.testeditor.releng.parent')

    // Set the display name for the job to the version
    String version = getCurrentVersion()
    currentBuild.displayName = version
    echo "Version to release is: $version"
}

String getCurrentVersion() {
    def pom = readMavenPom file: 'pom.xml'
    return pom.parent.version
}

String getGitUrl() {
    return scm.userRemoteConfigs.first().url
}

String getGitUrlAsSsh() {
    return getGitUrl().replace("https://github.com/", "git@github.com:")
}

void postRelease(String preReleaseVersion) {
    stage 'Tag release'

        def version = "v${getCurrentVersion()}"
        echo "Tagging release as $version"
        sh "git add *"
        sh "git commit -m '[release] $version'"
        sh "git tag $version"
        // workaround: cannot push without credentials using HTTPS => push using SSH
        sh "git remote set-url origin ${getGitUrlAsSsh()}"
        sh "git push origin master --tags"

    stage 'Increment develop version'
        sh "git checkout develop"
        sh "git fetch origin"
        sh "git reset --hard origin/develop"
        def developVersion = getCurrentVersion()
        if (developVersion == preReleaseVersion) {
            sh "git merge origin/master"
            def nextSnapshotVersion = '\\${parsedVersion.majorVersion}.\\${parsedVersion.nextMinorVersion}.0-SNAPSHOT'
            setVersion(nextSnapshotVersion, 'releng/org.testeditor.releng.target/pom.xml', 'org.testeditor.releng.target.parent')
            setVersion(nextSnapshotVersion, 'pom.xml', 'org.testeditor.releng.parent')
            sh "git add *"
            sh "git commit -m '[release] set version ${getCurrentVersion()}'"
            sh "git push origin develop"
        } else {
            echo "Version on develop not incremented as it differs from the preReleaseVersion."
        }
}

boolean isTag() {
    try {
        sh '''\
            #!/bin/sh
            git describe --exact-match --tags
        '''.stripIndent()
        return true
    } catch (Exception e) {
        echo e.toString()
        return false
    }
}

void setVersion(String newVersion, String rootPom = null, String artifacts = null) {
    withMavenEnv {
        def goals = 'build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version'
        def pom = rootPom ? "-f $rootPom " : ''
        sh "mvn $pom$goals -Dartifacts=$artifacts -DnewVersion=$newVersion -Dtycho.mode=maven"
    }
}

/** Calls Maven with the given argument and adds the -B (batch) and -V (version) flag. */
void mvn(String argument) {
    sh "mvn $argument -B -V"
}

void codecov(String codecovCredentialsId) {
    withEnv(["ghprbPullId=${env.CHANGE_ID}", "GIT_BRANCH=${env.BRANCH_NAME}"]) {
        withCredentials([[$class: 'StringBinding', credentialsId: codecovCredentialsId, variable: 'CODECOV_TOKEN']]) {
            sh """\
                #!/bin/bash
                bash <(curl -s https://codecov.io/bash) || echo "Codecov did not collect coverage reports"
            """.stripIndent()
        }
    }
}

void withXvfb(def body) {
    // TODO why do we have more than one installation on our Jenkins? If we had one we wouldn't need to specify the installationName
    wrap([$class: 'Xvfb', installationName: 'System', timeout: 2, screen: '1024x768x24', displayNameOffset: 1, autoDisplayName: true], body)
}

void withMavenEnv(List envVars = [], def body) {
    String jdkTool = tool name: 'jdk8', type: 'hudson.model.JDK'
    String mvnTool = tool name: 'Maven 3.2.5', type: 'hudson.tasks.Maven$MavenInstallation'
    List mvnEnv = [
        "PATH+JDK=${jdkTool}/bin",
        "JAVA_HOME=${jdkTool}",
        "PATH+MVN=${mvnTool}/bin",
        "MAVEN_HOME=${mvnTool}"
    ]
    mvnEnv.addAll(envVars)
    withEnv(mvnEnv) {
        body.call()
    }
}

/**
 * Workaround for Jenkins bug with feature branches (workspace has feature%2Fmy_feature in it).
 * See https://issues.jenkins-ci.org/browse/JENKINS-30744 (marked as resolved but still occurs).
 */
void nodeWithProperWorkspace(def body) {
    node {
        ws(getWorkspace()) {
            body.call()
        }
    }
}

def getWorkspace() {
    pwd().replace("%2F", "_")
}
