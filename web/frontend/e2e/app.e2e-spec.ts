import { TestEditorWebPage } from './app.po';

describe('test-editor-web App', () => {
  let page: TestEditorWebPage;

  beforeEach(() => {
    page = new TestEditorWebPage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
