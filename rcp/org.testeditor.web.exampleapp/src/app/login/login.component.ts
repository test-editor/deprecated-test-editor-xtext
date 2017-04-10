import { Component, OnInit } from '@angular/core';
import { Router } from "@angular/router";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

  userId: String;
  password: String;
  errorMessage: String;

  constructor(private router: Router ) { }

  ngOnInit() {
  }

  login() {
    if(this.userId=='max') {
      this.router.navigateByUrl("main")
    }else {
      this.errorMessage = "Login data invalid"
    }
  }

}
