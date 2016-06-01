#!groovy

nodeWithProperWorkspace {
    stage 'Checkout'
    checkout scm
    sh 'git clean -ffdx'

    stage 'Build target platform'
    withMavenEnv {
        sh 'mvn clean install -B -V -f "releng/org.testeditor.releng.target/pom.xml"'
    }

    stage 'Build'
    withMavenEnv(["MAVEN_OPTS=-Xms512m -Xmx2g"]) {
        withXvfb {
            sh "mvn clean install -B -V -Dmaven.test.failure.ignore -Dsurefire.useFile=false -Dtycho.localArtifacts=ignore"
        }
    }
    
    stage 'Collect test results'
    step([$class: 'JUnitResultArchiver', testResults: '**/target/surefire-reports/TEST-*.xml'])
    codecov('codecov_test-editor-xtext')
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
    wrap([$class: 'Xvfb', installationName: 'System', timeout: 0, screen: '1024x768x24'], body)
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