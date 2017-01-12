#!groovy
nodeWithProperWorkspace {

    stage('Checkout') {
        checkout scm
        if (isMaster()) {
            // git by default checks out detached, we need a local branch
            sh "git checkout $env.BRANCH_NAME" // workaround for https://issues.jenkins-ci.org/browse/JENKINS-31924
            sh 'git fetch --prune origin +refs/tags/*:refs/tags/*' // delete all local tags
            sh "git reset --hard origin/master"
            sh "git clean -ffdx"
        } else {
            sh "git clean -ffd"
        }
    }

    if (isMaster() && isVersionTag()) {
        // Workaround: we don't want infinite releases.
        echo "Aborting build as the current commit on master is already tagged."
        currentBuild.displayName = "checkout-only"
        return
    }

    def preReleaseVersion = getCurrentVersion()
    if (isMaster()) {
        prepareRelease()
    }

    stage('Build target platform') {
        withMavenEnv {
            gradle 'buildTarget'
        }
    }

    stage('Build Web Components') {
        withMavenEnv {
            gradle 'preBuildWeb'
        }
    }

    stage('Build') {
        withMavenEnv(["MAVEN_OPTS=-Xms512m -Xmx2g"]) {
            withXvfb {
                gradle 'build'
            }
        }
    }

    // workaround for now to speed-up the build: only build the product on develop, master and branches that end with -with-product
    def buildProduct = env.BRANCH_NAME == "develop" || env.BRANCH_NAME.endsWith("-with-product") || isMaster()
    if (buildProduct) {
        stage('Build product') {
            withMavenEnv(["MAVEN_OPTS=-Xms512m -Xmx2g"]) {
                gradle 'buildProduct'
            }
        }
    }

    if (isMaster()) {
        stage('Deploy') {
            withMavenEnv {
                gradle 'deploy'
            }
        }
        postRelease(preReleaseVersion)
    }

    stage('Archive results') {
        // archive all written screenshots
        archiveArtifacts artifacts: 'rcp/org.testeditor.rcp4.uatests/screenshots/**/*.png', fingerprint: true
        if (buildProduct) {
            archive '**/target/products/*.zip'
        }
        step([$class: 'JUnitResultArchiver', testResults: '**/target/surefire-reports/TEST-*.xml'])
        codecov('codecov_test-editor-xtext')
    }
}

void prepareRelease() {
    stage('Prepare release') {
        // Remove SNAPSHOT version
        echo 'Removing SNAPSHOT from target platform version'
        def String noSnapshotVersion = '\\${parsedVersion.majorVersion}.\\${parsedVersion.minorVersion}.\\${parsedVersion.incrementalVersion}'
        setVersion(noSnapshotVersion, 'target-platform/pom.xml', 'org.testeditor.releng.target.parent')
        echo 'Removing SNAPSHOT from test-editor version'
        setVersion(noSnapshotVersion, 'pom.xml', 'org.testeditor.releng.parent')

        // Set the display name for the job to the version
        String version = getCurrentVersion()
        currentBuild.displayName = version
        echo "Version to release is: $version"
    }
}

String getCurrentVersion() {
    def pom = readMavenPom file: 'pom.xml'
    return pom.parent.version
}

void postRelease(String preReleaseVersion) {
    stage('Tag release') {
        def version = "v${getCurrentVersion()}"
        echo "Tagging release as $version"
        sh "git add ."
        sh "git commit -m '[release] $version'"
        sh "git tag $version"
        // workaround: cannot push without credentials using HTTPS => push using SSH
        sh "git remote set-url origin ${getGithubUrlAsSsh()}"
        sh "git push origin master --tags"
    }

    stage('Increment develop version') {
        sh "git checkout develop"
        sh "git fetch origin"
        sh "git reset --hard origin/develop"
        def developVersion = getCurrentVersion()
        if (developVersion == preReleaseVersion) {
            sh "git merge origin/master"
            def nextSnapshotVersion = '\\${parsedVersion.majorVersion}.\\${parsedVersion.nextMinorVersion}.0-SNAPSHOT'
            setVersion(nextSnapshotVersion, 'target-platform/pom.xml', 'org.testeditor.releng.target.parent')
            setVersion(nextSnapshotVersion, 'pom.xml', 'org.testeditor.releng.parent')
            sh "git add ."
            sh "git commit -m '[release] set version ${getCurrentVersion()}'"
            sh "git push origin develop"
        } else {
            echo "Version on develop not incremented as it differs from the preReleaseVersion."
        }
    }
}

void setVersion(String newVersion, String rootPom = null, String artifacts = null) {
    withMavenEnv {
        def goals = 'build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version'
        def pom = rootPom ? "-f $rootPom " : ''
        sh "mvn $pom$goals -Dartifacts=$artifacts -DnewVersion=$newVersion -Dtycho.mode=maven"
    }
}
