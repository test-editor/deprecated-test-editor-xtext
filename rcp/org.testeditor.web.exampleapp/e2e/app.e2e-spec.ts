import { LoginExamplePage } from './app.po';

describe('login-example App', () => {
  let page: LoginExamplePage;

  beforeEach(() => {
    page = new LoginExamplePage();
  });

  it('should display message saying app works', () => {
    page.navigateTo();
    expect(page.getParagraphText()).toEqual('app works!');
  });
});
